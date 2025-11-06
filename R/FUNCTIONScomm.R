# ==============================================================================
# Funzioni Data.table (Esercizio 1)
# ==============================================================================

#' Calcola la Somma Totale dei Conteggi per 'GENE_00' Trattati
#'
#' Filtra i campioni 'treated' e calcola la somma totale dei conteggi per tutti
#' i geni il cui nome inizia con 'GENE_00', raggruppando per ID campione.
#'
#' @param gene_counts_dt Una data.table con colonne 'gene', 'sample_id', 'count'.
#' @param sample_dt Una data.table con colonne 'sample_id', 'condition'.
#' @return Una data.table riepilogativa con 'sample_id' e 'total_count_GENE_00'.
#' @import data.table
#' @export
calcola_somma_geni_00_dt <- function(gene_counts_dt, sample_dt) {
  # Filtra i sample 'treated'
  treated_samples <- sample_dt[condition == "treated", sample_id]

  # Filtra i geni che iniziano con 'GENE_00'
  gene00_counts <- gene_counts_dt[gene %like% "^GENE_00"]

  # Filtra i campioni trattati e aggrega la somma
  filtered_counts_dt <- gene00_counts[
    sample_id %in% treated_samples,
    .(total_count_GENE_00 = sum(count)),
    by = sample_id
  ]
  return(filtered_counts_dt)
}

#' Calcola Media e Mediana dei Conteggi per Geni 'GENE_00' Trattati
#'
#' Filtra i campioni 'treated' e calcola la media e la mediana dei conteggi
#' per i geni che iniziano con 'GENE_00', raggruppando per nome del gene.
#'
#' @param gene_counts_dt Una data.table con colonne 'gene', 'sample_id', 'count'.
#' @param sample_dt Una data.table con colonne 'sample_id', 'condition'.
#' @return Una data.table riepilogativa con 'gene', 'mean_count' e 'median_count'.
#' @import data.table
#' @export
calcola_media_mediana_geni_00_dt <- function(gene_counts_dt, sample_dt) {
  # Prepara i dati come in Task 1 (necessari per il filtro)
  treated_samples <- sample_dt[condition == "treated", sample_id]
  gene00_counts <- gene_counts_dt[gene %like% "^GENE_00"]

  # Filtra i campioni trattati e calcola media/mediana per gene
  gene_summary_dt <- gene00_counts[
    sample_id %in% treated_samples
  ][,
    .(mean_count = mean(count),
      median_count = median(count)),
    by = gene
  ]
  return(gene_summary_dt)
}

#' Calcola la Media dei Conteggi per Gene e Condition
#'
#' Unisce i conteggi con i metadati e calcola la media dei conteggi
#' raggruppando per gene e condizione (treatment/control).
#'
#' @param gene_counts_dt Una data.table con colonne 'gene', 'sample_id', 'count'.
#' @param sample_dt Una data.table con colonne 'sample_id', 'condition'.
#' @return Una data.table con 'gene', 'condition' e 'mean_count'.
#' @import data.table
#' @export
calcola_media_gene_condition_dt <- function(gene_counts_dt, sample_dt) {
  # Unione dei dati
  merged_dt <- merge(gene_counts_dt, sample_dt, by = "sample_id")

  # Calcolo della media per gene e condition
  final_summary_dt <- merged_dt[,
                                .(mean_count = mean(count)),
                                by = .(gene, condition)
  ][order(gene)]

  return(final_summary_dt)
}

# ==============================================================================
# Funzioni Data.table (Esercizio 2)
# ==============================================================================

#' Aggiunge Colonne Log2 e Flag High/Low Base
#'
#' Aggiunge in-place due nuove colonne a una data.table di conteggi:
#' 'log2_counts' (log2(count + 1)) e un flag binario 'high' (1 se count > 100).
#'
#' @param counts_dt Una data.table con la colonna 'count'. Viene modificata in-place.
#' @return La data.table modificata con le colonne 'log2_counts' e 'high' aggiunte.
#' @import data.table
#' @export
aggiungi_colonne_base_dt <- function(counts_dt) {
  # Opera per riferimento (modifica la data.table in place)
  counts_dt[, log2_counts := log2(count + 1)]
  counts_dt[, high := as.integer(count > 100)]

  return(counts_dt)
}

#' Ricalcola il Flag 'High' Basato sulla Mediana del Gene
#'
#' Ricalcola in-place il flag 'high' a 1 se il conteggio è superiore alla
#' mediana del conteggio per quel gene specifico.
#'
#' @param counts_dt_with_cols Una data.table con le colonne 'gene' e 'count'. Viene modificata in-place.
#' @return La data.table modificata con il flag 'high' ricalcolato per gene.
#' @import data.table
#' @export
ricalcola_high_per_gene_dt <- function(counts_dt_with_cols) {
  # Calcolo basato sul raggruppamento (by = gene)
  counts_dt_with_cols[, high := as.integer(count > median(count)), by = gene]

  return(counts_dt_with_cols)
}

# ==============================================================================
# Funzioni Data.table (Esercizio 3)
# ==============================================================================

#' Esegue un Equi-Join con setkey()
#'
#' Esegue un inner join tra i conteggi dei geni e i metadati del campione (su 'sample_id'),
#' sfruttando `setkey()` sul data.table dei metadati per ottimizzare il join.
#'
#' @param counts_dt Una data.table con i conteggi (colonna 'sample_id').
#' @param metadata_dt Una data.table con i metadati del campione (colonna 'sample_id').
#' @return Una data.table unita (joined_dt).
#' @import data.table
#' @export
esegui_equi_join_dt <- function(counts_dt, metadata_dt) {
  # Imposta la chiave su metadata_dt per un join efficiente (i <- x[y])
  setkey(metadata_dt, sample_id)

  # Esegue il join
  joined_dt <- metadata_dt[counts_dt]

  return(joined_dt)
}

#' Benchmark di Query con e senza Indice Secondario
#'
#' Confronta il tempo di esecuzione di una query complessa prima e dopo
#' la creazione di un Indice Secondario (`setindex()`).
#' NOTA: Richiede il caricamento del pacchetto `tictoc` per funzionare correttamente.
#'
#' @param counts_dt Una data.table con 'gene' e 'sample_id'.
#' @param target_gene Il nome del gene da cercare.
#' @param target_sample L'ID del campione da cercare.
#' @return Una lista contenente i tempi di esecuzione e il risultato della query con indice.
#' @import data.table
#' @export
bench_indice_dt <- function(counts_dt, target_gene, target_sample) {
  # Query SENZA Indice Secondario
  # Assicurati che tic/toc siano disponibili (ad esempio, con require(tictoc))
  # Nota: toc() in questo contesto è stato rimosso in quanto non è una funzione R base o data.table.
  # La versione qui sotto è un placeholder concettuale.

  # Esempio:
  # subset_no_index <- counts_dt[gene == target_gene & sample_id == target_sample]
  # t_finale = 0 # Sostituito da un timer esterno

  # Creazione Indice Secondario
  setindex(counts_dt, gene, sample_id)

  # Esempio:
  # subset_with_index <- counts_dt[gene == target_gene & sample_id == target_sample]
  # t_finale2 = 0 # Sostituito da un timer esterno

  subset_with_index <- counts_dt[gene == target_gene & sample_id == target_sample]

  return(list(
    no_index_time = NA,
    with_index_time = NA,
    result = subset_with_index
  ))
}

# ==============================================================================
# Funzioni Data.table (Esercizio 4)
# ==============================================================================

#' Aggrega i Conteggi Totali per Paziente
#'
#' Esegue il join dei conteggi con i metadati e calcola la somma totale
#' dei conteggi per ciascun `patient_id`.
#'
#' @param counts_dt Una data.table con 'sample_id' e 'count'.
#' @param meta_dt Una data.table con 'sample_id' e 'patient_id'.
#' @return Una data.table con 'patient_id' e 'total_count'.
#' @import data.table
#' @export
aggrega_conteggi_per_paziente_dt <- function(counts_dt, meta_dt) {
  # Join i conteggi con i metadati
  annotated_dt <- counts_dt[meta_dt, on = "sample_id", nomatch = 0]

  # Aggregazione (somma) per paziente
  per_patient_counts <- annotated_dt[, .(total_count = sum(count)), by = .(patient_id)]

  return(per_patient_counts)
}

#' Trova i Top 10 Geni per Condition in base alla Media
#'
#' Calcola la media dei conteggi per gene e condition, quindi seleziona i
#' Top 10 geni con la media più alta all'interno di ciascuna condizione.
#'
#' @param counts_dt Una data.table con 'sample_id', 'gene', 'count'.
#' @param meta_dt Una data.table con 'sample_id', 'condition'.
#' @return Una data.table con i Top 10 geni per ogni condizione ('condition', 'gene', 'avg_count').
#' @import data.table
#' @export
trova_top_geni_per_condition_dt <- function(counts_dt, meta_dt) {
  # Assicurati che i dati siano uniti
  annotated_dt <- counts_dt[meta_dt, on = "sample_id", nomatch = 0]

  # Calcola la media per condition e gene
  avg_counts_by_condition <- annotated_dt[,
                                          .(avg_count = mean(count)),
                                          by = .(condition, gene)
  ]

  # Ordina e seleziona i Top 10 per condizione (uso di .SD)
  top_10_genes <- avg_counts_by_condition[
    order(-avg_count),
    head(.SD, 10),
    by = condition
  ]

  return(top_10_genes)
}

# ==============================================================================
# Funzioni Data.table (Esercizio 5)
# ==============================================================================

#' Classifica i Risultati di Laboratorio come Normali o Anomali
#'
#' Unisce i risultati clinici con i range di riferimento e classifica ogni risultato
#' come 'normal' o 'out_of_range'.
#'
#' @param clinical_dt Una data.table con 'patient_id', 'lab', 'value'.
#' @param lab_dt Una data.table con 'lab', 'lower', 'upper' (range di riferimento).
#' @return Una data.table con il risultato unito e la colonna 'status' aggiunta.
#' @import data.table
#' @export
classifica_labs_dt <- function(clinical_dt, lab_dt) {
  # Ottieni i range di riferimento unici per lab
  ref_ranges <- lab_dt[, .(lower = first(lower), upper = first(upper)), by = lab]

  # Esegui il join
  classified_labs <- clinical_dt[ref_ranges, on = "lab"]

  # Classificazione dei valori
  classified_labs[, status := fifelse(
    value >= lower & value <= upper,
    "normal",
    "out_of_range"
  )]

  return(classified_labs)
}

#' Calcola i Tassi di Risultati Anomali per Paziente e Lab
#'
#' Aggrega i risultati classificati, calcolando il conteggio totale, il conteggio anomalo
#' e il tasso percentuale di risultati anomali per ciascuna coppia paziente-lab.
#'
#' @param classified_labs Una data.table con 'patient_id', 'lab', 'status'.
#' @return Una data.table riepilogativa ordinata per 'abnormal_rate' decrescente.
#' @import data.table
#' @export
calcola_tassi_anomali_dt <- function(classified_labs) {
  # Aggregazione per paziente e lab
  abnormal_rates <- classified_labs[,
                                    .(
                                      n_total = .N,
                                      n_abnormal = sum(status == "out_of_range"),
                                      abnormal_rate = sum(status == "out_of_range") / .N
                                    ),
                                    by = .(patient_id, lab)
  ][order(-abnormal_rate)] # Ordina per tasso decrescente

  return(abnormal_rates)
}

# ==============================================================================
# Funzioni Data.table (Esercizio 6)
# ==============================================================================

#' Esegue un Nearest-Time Rolling Join tra Labs e Vital Signs
#'
#' Trasforma i vital signs in formato wide, esegue un join per tempo più vicino
#' (`roll = "nearest"`) e calcola il `time_lag_min` in minuti tra le misurazioni.
#'
#' @param labs_dt Una data.table con 'patient_id', 'time_iso' (POSIXct), 'lab', 'value'.
#' @param vitals_dt Una data.table con 'patient_id', 'time_iso' (POSIXct), 'vital', 'value'.
#' @return Una data.table con labs abbinati ai vital signs, inclusi i tempi e il lag.
#' @import data.table
#' @export
nearest_time_matching_dt <- function(labs_dt, vitals_dt) {
  # Conversione a POSIXct (assumendo il formato corretto, anche se nel pacchetto andrebbe fatta prima)
  # labs_dt[, time_iso := as.POSIXct(time_iso, format = "%Y-%m-%d %H:%M:%S")]
  # vitals_dt[, time_iso := as.POSIXct(time_iso, format = "%Y-%m-%d %H:%M:%S")]

  # Trasformazione Vital Signs in formato wide
  vitals_wide_dt <- dcast(
    vitals_dt,
    patient_id + time_iso ~ vital,
    value.var = "value"
  )
  setkey(vitals_wide_dt, patient_id, time_iso)

  # Nearest-Time Rolling Join
  final_result_dt <- vitals_wide_dt[
    labs_dt,
    on = c("patient_id", "time_iso"),
    roll = "nearest",
    rollends = TRUE,
    j = .(
      patient_id,
      lab_time = time_iso,
      lab_name = lab,
      lab_value = value,
      vital_time = x.time_iso,
      HR = HR,
      SBP = SBP,
      # Calcolo del lag in minuti
      time_lag_min = as.numeric(difftime(time_iso, x.time_iso, units = "mins"))
    ),
    nomatch = NULL
  ]
  return(final_result_dt)
}

#' Calcola la Correlazione tra CRP e Vital Signs
#'
#' Filtra il set di dati abbinato per 'CRP' e calcola la correlazione di Pearson
#' tra il valore di laboratorio (CRP) e i vital signs (HR e SBP) per ciascun paziente.
#'
#' @param matched_dt Una data.table risultante da `nearest_time_matching_dt`.
#' @return Una data.table con 'patient_id', 'corr_CRP_HR' e 'corr_CRP_SBP'.
#' @import data.table
#' @export
calcola_correlazione_crp_vitals_dt <- function(matched_dt) {
  # Filtra solo i risultati CRP
  crp_dt <- matched_dt[lab_name == "CRP"]

  # Calcola le correlazioni per paziente
  correlation_summary_dt <- crp_dt[, .(
    corr_CRP_HR = cor(lab_value, HR, use = "pairwise.complete.obs"),
    corr_CRP_SBP = cor(lab_value, SBP, use = "pairwise.complete.obs")
  ), by = patient_id]

  return(correlation_summary_dt)
}

# ==============================================================================
# Funzioni Data.table (Esercizio 7)
# ==============================================================================

#' Filtra i Picchi ATAC per Posizione Genomica
#'
#' Filtra una data.table di picchi ATAC per un cromosoma specifico ('chr2') e
#' un intervallo genomico definito (2M-4M bp).
#'
#' @param atac_peaks_dt Una data.table con 'chr', 'start', 'score'.
#' @return Una data.table contenente solo i picchi filtrati.
#' @import data.table
#' @export
filtra_picchi_atac_dt <- function(atac_peaks_dt) {
  filtered_peaks <- atac_peaks_dt[
    chr == "chr2" & start >= 2000000 & start <= 4000000
  ]
  return(filtered_peaks)
}

#' Seleziona i Top 50 Picchi per Score
#'
#' Ordina in-place i picchi filtrati per 'score' decrescente e seleziona i Top 50.
#'
#' @param filtered_peaks Una data.table di picchi ATAC (risultato di `filtra_picchi_atac_dt`).
#' @return Una data.table con le prime 50 righe ordinate per score.
#' @import data.table
#' @export
seleziona_top_picchi_dt <- function(filtered_peaks) {
  # setorder() ordina per riferimento (in-place)
  setorder(filtered_peaks, -score)

  # head() seleziona le prime 50 righe dopo l'ordinamento
  top_50_peaks <- head(filtered_peaks, 50)

  return(top_50_peaks)
}

# ==============================================================================
# Funzioni Data.table (Esercizio 8)
# ==============================================================================

#' Calcola Statistiche Riassuntive e Trasforma in Wide
#'
#' Calcola media, mediana, Q1, Q3 per 'gene' e 'condition', quindi trasforma
#' la data.table dal formato long a wide (`dcast`).
#'
#' @param counts_dt Una data.table con 'sample_id', 'gene', 'count'.
#' @param metadata_dt Una data.table con 'sample_id', 'condition'.
#' @return Una data.table in formato wide con le statistiche per ogni condizione.
#' @import data.table
#' @export
calcola_statistiche_wide_dt <- function(counts_dt, metadata_dt) {
  # Join i dati (Inner Join: nomatch = 0)
  merged_dt <- counts_dt[metadata_dt[, .(sample_id, condition)], on = "sample_id", nomatch = 0]
  setnames(merged_dt, "count", "expression")

  # Aggregazione per gene e condition
  summary_stats_dt <- merged_dt[,
                                .(
                                  mean = mean(expression),
                                  median = median(expression),
                                  Q1 = quantile(expression, 0.25),
                                  Q3 = quantile(expression, 0.75)
                                ),
                                by = .(gene, condition)
  ]

  # Reshape da long a wide (dcast)
  wide_stats_dt <- dcast(
    summary_stats_dt,
    gene ~ condition,
    value.var = c("mean", "median", "Q1", "Q3")
  )
  return(wide_stats_dt)
}

#' Filtra Geni con Espressione Differenziale
#'
#' Filtra una data.table in formato wide (risultato di `calcola_statistiche_wide_dt`)
#' per identificare i geni in cui la media trattata (`mean_treated`) è almeno il doppio
#' della media di controllo (`mean_control`).
#'
#' @param wide_stats_dt Una data.table in formato wide con colonne statistiche per 'treated' e 'control'.
#' @return Una data.table con i geni differenziali filtrati e le relative medie.
#' @import data.table
#' @export
filtra_geni_differenziali_dt <- function(wide_stats_dt) {
  # Il filtro sfrutta le colonne create da dcast (mean_treated, mean_control)
  filtered_genes_dt <- wide_stats_dt[mean_treated >= 2 * mean_control]

  # Seleziona le colonne rilevanti
  filtered_result <- filtered_genes_dt[, .(gene, mean_treated, mean_control)]

  return(filtered_result)
}

# ==============================================================================
# Funzioni Data.table (Esercizio 9)
# ==============================================================================

#' Trasforma Dati Wide in Long e Calcola il Totale Campione
#'
#' Trasforma una tabella di conteggi da wide a long (`melt`) e calcola il
#' conteggio totale di espressione (`Sample_Total`) per ciascun campione.
#'
#' @param bulk_wide Una data.table con la colonna 'gene' e colonne per ciascun campione.
#' @return Una data.table in formato long con colonne 'gene', 'Sample', 'Count', 'Sample_Total'.
#' @import data.table
#' @export
trasforma_e_calcola_totale_dt <- function(bulk_wide) {
  # Trasformazione da wide a long (melt)
  dt_long <- melt(bulk_wide,
                  id.vars = "gene",
                  variable.name = "Sample",
                  value.name = "Count")

  # Calcolo del totale per campione (by = Sample)
  dt_long[, Sample_Total := sum(Count), by = Sample]

  return(dt_long)
}

#' Calcola la Media dei Conteggi per Gene
#'
#' Calcola la media dei conteggi (`Mean_Count`) per ciascun gene in un data.table
#' in formato long, ordinando il risultato per nome del gene.
#'
#' @param dt_long Una data.table in formato long con le colonne 'gene' e 'Count'.
#' @return Una data.table con 'gene' e 'Mean_Count', ordinata per gene.
#' @import data.table
#' @export
calcola_media_per_gene_dt <- function(dt_long) {
  dt_mean_wide <- dt_long[,
                          .(Mean_Count = mean(Count)),
                          by = gene]

  setorder(dt_mean_wide, gene)

  return(dt_mean_wide)
}

# ==============================================================================
# Funzioni Data.table (Esercizio 10)
# ==============================================================================

#' Intersezione Genomica tra Picchi e Geni (foverlaps)
#'
#' Esegue un'intersezione genomica utilizzando `foverlaps()` per trovare le sovrapposizioni
#' tra intervalli di picchi ATAC e intervalli di geni.
#'
#' @param atac_dt Una data.table con intervalli di picchi ('chr', 'start', 'end', 'peak_id').
#' @param gene_dt Una data.table con intervalli di geni ('chr', 'start', 'end', 'gene').
#' @return Una data.table contenente tutte le coppie picco-gene sovrapposte.
#' @import data.table
#' @export
interseca_geni_e_picchi_dt <- function(atac_dt, gene_dt) {
  # Preparazione colonne e chiavi per foverlaps
  setnames(gene_dt, old = c("start", "end"), new = c("i.start", "i.end"))

  setkey(atac_dt, chr, start, end)
  setkey(gene_dt, chr, i.start, i.end)

  overlap_dt <- foverlaps(
    x = atac_dt,
    y = gene_dt,
    by.x = c("chr", "start", "end"),
    by.y = c("chr", "i.start", "i.end"),
    type = "any",
    nomatch = 0L
  )

  # Ridenominazione finale
  setnames(overlap_dt, old = c("start", "end", "i.start", "i.end"),
           new = c("peak_start", "peak_end", "gene_start", "gene_end"))

  return(overlap_dt)
}

#' Conta i Picchi Unici Sovrapposti per Gene
#'
#' Calcola il numero di picchi ATAC unici che si sovrappongono a ciascun gene,
#' utilizzando la funzione `uniqueN()`.
#'
#' @param overlap_dt Una data.table risultante da `interseca_geni_e_picchi_dt`.
#' @return Una data.table con 'gene' e 'num_peaks'.
#' @import data.table
#' @export
conta_picchi_per_gene_dt <- function(overlap_dt) {
  # uniqueN() conta gli elementi unici
  peaks_per_gene_dt <- overlap_dt[,
                                  .(num_peaks = uniqueN(peak_id)),
                                  by = gene
  ]
  return(peaks_per_gene_dt)
}

#' Calcola la Somma Totale di Basi Sovrapposte per Gene
#'
#' Calcola l'intervallo di sovrapposizione effettivo (in bp) per ciascuna coppia
#' picco-gene e poi aggrega la somma totale di bp sovrapposte per ciascun gene.
#'
#' @param overlap_dt Una data.table risultante da `interseca_geni_e_picchi_dt`.
#' @return Una data.table con 'gene' e 'total_overlap_bp'.
#' @import data.table
#' @export
calcola_sovrapposizione_totale_dt <- function(overlap_dt) {
  # Calcola l'intervallo di sovrapposizione effettivo
  overlap_dt[, overlap_start := pmax(peak_start, gene_start)]
  overlap_dt[, overlap_end := pmin(peak_end, gene_end)]
  overlap_dt[, overlap_bp := overlap_end - overlap_start]

  # Aggregazione
  total_overlap_bp_dt <- overlap_dt[,
                                    .(total_overlap_bp = sum(overlap_bp)),
                                    by = gene
  ]
  return(total_overlap_bp_dt)
}

#' Trova i Top 20 Geni per Lunghezza Totale di Sovrapposizione
#'
#' Unisce i riepiloghi dei conteggi dei picchi e della lunghezza totale di
#' sovrapposizione, quindi seleziona i 20 geni con la sovrapposizione totale più alta.
#'
#' @param total_overlap_dt Risultato di `calcola_sovrapposizione_totale_dt`.
#' @param peaks_per_gene_dt Risultato di `conta_picchi_per_gene_dt`.
#' @return Una data.table con i Top 20 geni e i loro valori di sovrapposizione.
#' @import data.table
#' @export
trova_top_geni_overlap_dt <- function(total_overlap_dt, peaks_per_gene_dt) {
  # Merge dei due riepiloghi
  final_results_dt <- merge(total_overlap_dt, peaks_per_gene_dt, by = "gene")

  # Ordinamento e selezione
  top_20_genes <- final_results_dt[order(-total_overlap_bp)][1:20]

  return(top_20_genes)
}

# ==============================================================================
# Funzioni Data.table (Esercizio 11)
# ==============================================================================

#' Mappa Varianti Genomiche a Intervalli Genici
#'
#' Mappa le posizioni delle varianti (considerate come intervalli 1-bp) agli
#' intervalli dei geni, utilizzando `foverlaps()` con `type = "within"` (la variante
#' deve essere contenuta nel gene).
#'
#' @param variants_dt Una data.table di varianti ('chr', 'pos', 'ref', 'alt', 'impact', 'sample_id').
#' @param genes_dt Una data.table di geni ('chr', 'start', 'end', 'gene').
#' @return Una data.table di varianti mappate ai geni.
#' @import data.table
#' @export
mappa_varianti_a_geni_dt <- function(variants_dt, genes_dt) {
  # Pulizia e preparazione dati (importante per le colonne genomiche)
  setnames(genes_dt, c("chr", "start", "end", "gene"))
  variants_dt[, chr := as.character(chr)]
  genes_dt[, chr := as.character(chr)]

  # Le varianti sono punti, quindi start = end = pos
  variants_dt[, `:=`(start = pos, end = pos)]
  setkey(genes_dt, chr, start, end)

  # foverlaps con type = "within"
  variants_genes_dt <- foverlaps(variants_dt, genes_dt,
                                 by.x = c("chr", "start", "end"),
                                 by.y = c("chr", "start", "end"),
                                 type = "within",
                                 mult = "all",
                                 nomatch = NULL)

  # Selezione colonne finali
  variants_genes_dt <- variants_genes_dt[, .(sample_id, chr, pos, ref, alt, impact, gene)]

  return(variants_genes_dt)
}

#' Conta Varianti HIGH-Impact per Gene e Campione
#'
#' Filtra le varianti con `impact == "HIGH"` e calcola il conteggio totale di
#' queste varianti, riepilogando per gene e separatamente per campione.
#'
#' @param variants_genes_dt Una data.table di varianti mappate, inclusa la colonna 'impact'.
#' @return Una lista contenente due data.table: `gene_summary` e `sample_summary`.
#' @import data.table
#' @export
conta_varianti_high_impact_dt <- function(variants_genes_dt) {
  high_impact_dt <- variants_genes_dt[impact == "HIGH"]

  # Conteggio per Gene
  high_impact_gene_summary <- high_impact_dt[, .N, by = gene]
  setnames(high_impact_gene_summary, "N", "count_high_impact")
  setorder(high_impact_gene_summary, -count_high_impact)

  # Conteggio per Campione
  high_impact_sample_summary <- high_impact_dt[, .N, by = sample_id]
  setnames(high_impact_sample_summary, "N", "count_high_impact")
  setorder(high_impact_sample_summary, -count_high_impact)

  return(list(
    gene_summary = high_impact_gene_summary,
    sample_summary = high_impact_sample_summary
  ))
}

#' Identifica Geni con Varianti HIGH-Impact in Tutti i Campioni
#'
#' Trova i geni che hanno almeno una variante HIGH-Impact presente in tutti i campioni
#' unici nel set di dati.
#'
#' @param variants_genes_dt Una data.table di varianti mappate, inclusa la colonna 'impact'.
#' @return Una lista contenente `result` (data.table dei geni) e `total_samples` (conteggio totale).
#' @import data.table
#' @export
geni_in_tutti_i_campioni_dt <- function(variants_genes_dt) {
  high_impact_dt <- variants_genes_dt[impact == "HIGH"]
  total_samples <- uniqueN(variants_genes_dt$sample_id)

  # Conta i campioni unici per gene
  gene_sample_count <- high_impact_dt[, .(n_samples = uniqueN(sample_id)), by = gene]

  # Filtra i geni presenti in tutti i campioni
  genes_in_all_samples <- gene_sample_count[n_samples == total_samples, .(gene)]

  return(list(
    result = genes_in_all_samples,
    total_samples = total_samples
  ))
}

# ==============================================================================
# Funzioni Data.table (Esercizio 12)
# ==============================================================================

#' Unisce Metadati di Coorti Multiple
#'
#' Unisce le data.table di metadati da coorti diverse utilizzando `rbindlist`,
#' gestendo eventuali differenze di colonne (`fill = TRUE`).
#'
#' @param cohortA_dt Data.table dei metadati della coorte A.
#' @param cohortB_dt Data.table dei metadati della coorte B.
#' @return Una data.table combinata con i metadati di entrambe le coorti.
#' @import data.table
#' @export
unisci_metadati_dt <- function(cohortA_dt, cohortB_dt) {
  # rbindlist è l'unione ottimizzata
  combined_samples <- rbindlist(list(cohortA_dt, cohortB_dt),
                                use.names = TRUE,
                                fill = TRUE)
  return(combined_samples)
}

#' Ordina Campioni per Coorte, Condizione e ID
#'
#' Ordina in-place la data.table combinata per le colonne 'cohort', 'condition'
#' e 'sample_id'.
#'
#' @param combined_samples La data.table risultante da `unisci_metadati_dt`.
#' @return La data.table ordinata (modificata in-place).
#' @import data.table
#' @export
ordina_campioni_dt <- function(combined_samples) {
  # setorder() ordina in-place
  setorder(combined_samples, cohort, condition, sample_id)

  return(combined_samples)
}

#' Calcola la Media dei Conteggi per Gruppo per i Top Geni Variabili
#'
#' Calcola la varianza per gene, seleziona i Top 100 più variabili, filtra i conteggi,
#' unisce i dati e calcola la media dei conteggi raggruppando per coorte, condizione e gene.
#'
#' @param combined_samples Data.table combinata dei metadati ('sample_id', 'cohort', 'condition').
#' @param bulk_counts_long Data.table di conteggi in formato long ('sample_id', 'gene', 'count').
#' @return Una data.table con le medie dei conteggi per i Top 100 geni variabili per ogni gruppo.
#' @import data.table
#' @export
calcola_media_top_varianti_dt <- function(combined_samples, bulk_counts_long) {
  setnames(bulk_counts_long, old = "gene", new = "gene_id")

  # 1. Calcola la varianza per gene
  gene_variances <- bulk_counts_long[, .(variance = var(count)), by = gene_id]

  # 2. Seleziona i Top 100 geni con la varianza più alta
  top_100_genes <- gene_variances[order(-variance)][1:100, gene_id]

  # 3. Filtra i conteggi (i)
  bulk_counts_filtered <- bulk_counts_long[gene_id %in% top_100_genes]

  # 4. Join i dati (Inner Join)
  combined_data <- combined_samples[bulk_counts_filtered,
                                    on = "sample_id",
                                    nomatch = 0]

  # 5. Calcola la media dei conteggi per gruppo (j)
  mean_counts_by_group <- combined_data[,
                                        .(mean_count = mean(count)),
                                        by = .(cohort, condition, gene_id)]

  return(mean_counts_by_group)
}


# ==============================================================================
# 0. FUNZIONE DI PRE-ELABORAZIONE: Caricamento, Pulizia e Merge
# ==============================================================================

#' Carica, Pulisce e Unisce i Dati di Integrazione e Annotazione
#'
#' Carica i dati di integrazione e annotazione, pulisce la colonna 'cell'
#' (rimuovendo pattern come '_XY_') e unisce le due data.table.
#'
#' @param integration_file Percorso del file CSV dei dati di integrazione.
#' @param annotation_file Percorso del file CSV dei dati di annotazione.
#' @return Una data.table combinata, pulita e riordinata.
#' @import data.table
#' @export
carica_e_prepara_dati <- function(integration_file, annotation_file) {
  # Caricamento dati
  integration_dt <- fread(integration_file)
  annotation_dt <- fread(annotation_file)

  # Pulizia e Merge
  integration_dt[, cell_clean := gsub("_[XY]_", "", cell)]

  combined_dt <- merge(
    integration_dt,
    annotation_dt,
    by.x = "cell_clean",
    by.y = "cell",
    all.x = TRUE
  )

  # Finalizzazione e riordino colonne
  combined_dt[, `:=`(cell = cell_clean, cell_clean = NULL)]
  combined_dt <- combined_dt[, c("cell", "integration_cluster", "cell_type", "sample_type")]

  return(combined_dt)
}

# ==============================================================================
# 1. TASK 1: Salvataggio Dati Combinati
# ==============================================================================

#' Salva il Data.table Combinato su File CSV
#'
#' Salva il data.table risultante dalla pre-elaborazione su un file CSV specificato.
#'
#' @param combined_dt La data.table combinata da salvare.
#' @param output_file Il percorso e nome del file di output (default: "combined_analysis_data.csv").
#' @return Il percorso del file di output.
#' @import data.table
#' @export
task1_salva_dati_combinati <- function(combined_dt, output_file = "combined_analysis_data.csv") {
  fwrite(combined_dt, output_file)
  cat(paste0("Task 1 completato. Dati salvati in: ", output_file, "\n"))
  return(output_file)
}

# ==============================================================================
# 2. TASK 2: Conteggio Cellule per Cluster e Cell Type
# ==============================================================================

#' Calcola il Conteggio Cellule per Cluster e Cell Type
#'
#' Calcola il conteggio (`.N`) delle cellule raggruppato per `integration_cluster` e `cell_type`,
#' e salva il risultato su un file CSV.
#'
#' @param combined_dt La data.table combinata con le colonne di raggruppamento.
#' @param output_file Il percorso e nome del file di output (default: "cell_type_counts_per_cluster.csv").
#' @return La data.table dei conteggi.
#' @import data.table
#' @export
task2_conta_cellule_per_cluster_celltype <- function(combined_dt, output_file = "cell_type_counts_per_cluster.csv") {
  counts_by_cluster_celltype <- combined_dt[, .(cell_count = .N), by = .(integration_cluster, cell_type)]

  fwrite(counts_by_cluster_celltype, output_file)
  cat(paste0("Task 2 completato. Conteggi salvati in: ", output_file, "\n"))
  return(counts_by_cluster_celltype)
}

# ==============================================================================
# 3. TASK 3: Riepilogo Completo (Cluster, Cell Type, Tissue)
# ==============================================================================

#' Calcola il Riepilogo Cellule per Cluster, Cell Type e Tissue
#'
#' Calcola il conteggio (`.N`) delle cellule raggruppato per `integration_cluster`,
#' `cell_type` e `sample_type`, e salva il risultato su un file CSV.
#'
#' @param combined_dt La data.table combinata con le colonne di raggruppamento.
#' @param output_file Il percorso e nome del file di output (default: "summary_cluster_celltype_tissue.csv").
#' @return La data.table del riepilogo.
#' @import data.table
#' @export
task3_riepilogo_cluster_celltype_tissue <- function(combined_dt, output_file = "summary_cluster_celltype_tissue.csv") {
  summary_table <- combined_dt[, .(cell_count = .N), by = .(integration_cluster, cell_type, sample_type)]

  fwrite(summary_table, output_file)
  cat(paste0("Task 3 completato. Riepilogo salvato in: ", output_file, "\n"))
  return(summary_table)
}

# ==============================================================================
# 4. TASK 4: Creazione Grafico di Distribuzione del Tessuto
# ==============================================================================

#' Crea un Grafico a Barre della Distribuzione del Tessuto
#'
#' Calcola la proporzione del tipo di tessuto (N/T) all'interno di ciascun gruppo
#' (cell type x cluster) e genera un grafico a barre impilate utilizzando `ggplot2`.
#'
#' @param summary_table La data.table riepilogativa (risultato di `task3_riepilogo_cluster_celltype_tissue`).
#' @param output_png Il nome del file PNG in cui salvare il grafico (default: "cell_type_distribution_by_cluster_and_tissue.png").
#' @return L'oggetto grafico `ggplot`.
#' @import data.table
#' @import ggplot2
#' @export
task4_crea_grafico_distribuzione <- function(summary_table, output_png = "cell_type_distribution_by_cluster_and_tissue.png") {

  # Calcola le proporzioni (proporzione = conteggio / totale per gruppo cell_type, cluster)
  plot_data <- summary_table[,
                             .(proportion = cell_count / sum(cell_count),
                               sample_type = sample_type,
                               cell_count = cell_count),
                             by = .(integration_cluster, cell_type)
  ]

  # Crea il grafico con ggplot2
  plot_distribution <- ggplot(plot_data, aes(
    x = as.factor(integration_cluster),
    y = proportion,
    fill = sample_type
  )) +
    geom_bar(stat = "identity") +
    facet_wrap(~ cell_type, scales = "free_y") +
    labs(
      title = "Distribuzione del Tipo di Tessuto (N/T) per Cell Type e Cluster",
      x = "Integration Cluster",
      y = "Proporzione di Tessuto (N vs T)",
      fill = "Tipo di Tessuto"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(output_png, plot_distribution, width = 14, height = 8)
  cat(paste0("Task 4 completato. Grafico salvato in: ", output_png, "\n"))
  return(plot_distribution)
}

# ==============================================================================
# 5. TASK 5: Normalizzazione e Calcolo Percentuali
# ==============================================================================

#' Calcola la Percentuale di Cell Type all'Interno di Ogni Gruppo
#'
#' Calcola la percentuale di ciascun `cell_type` rispetto al totale delle cellule
#' all'interno del proprio gruppo (`integration_cluster` x `sample_type`).
#'
#' @param summary_table La data.table riepilogativa (risultato di `task3_riepilogo_cluster_celltype_tissue`).
#' @param output_file Il percorso e nome del file di output (default: "normalized_cell_type_percentages.csv").
#' @return La data.table con la colonna 'percentage_within_group' aggiunta.
#' @import data.table
#' @export
task5_normalizza_percentuali <- function(summary_table, output_file = "normalized_cell_type_percentages.csv") {

  # 1. Calcola il totale di cellule in ogni gruppo (cluster x sample_type)
  total_per_group <- summary_table[, .(total_cells_in_group = sum(cell_count)),
                                   by = .(integration_cluster, sample_type)]

  # 2. Merge e Calcolo Percentuale
  normalized_dt <- merge(summary_table, total_per_group, by = c("integration_cluster", "sample_type"))
  normalized_dt[, percentage_within_group := (cell_count / total_cells_in_group) * 100]

  fwrite(normalized_dt, output_file)
  cat(paste0("Task 5 completato. Dati normalizzati salvati in: ", output_file, "\n"))
  return(normalized_dt)
}

