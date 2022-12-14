library(tidyverse)
library(furrr)
library(tictoc)
library(tidymodels)
library(survival)
library(survminer)
library(glmnet)
library(granatlib)
library(knitr)

NiceName <- function(x, .rmTRUE = TRUE, as_sentence  = TRUE) {
  if (.rmTRUE) {
    x <- str_remove(x, "TRUE")
  }
  
  out <- case_when(
    x == "price_diff" ~ "Value for money",
    x == "offer_price" ~ "Offer price",
    x == "fair_price" ~ "Predicted price",
    x == "id" ~ x,
    x == "brand" ~ x,
    x == "evjarat" ~ "year of manufacture",
    x == "allapot" ~ "condition",
    x == "allapotÚjszerű" ~ "condition: novel",
    x == "allapotSérülésmentes" ~ "condition: undamaged",
    x == "allapotNormál" ~ "condition: normal",
    x == "allapotMegkímélt" ~ "condition: spared",
    x == "kivitel" ~ "design", # TODO
    x == "kilometerora_allasa" ~ "mileage",
    x == "szallithato_szem_szama" ~ "number of persons transported",
    x == "ajtok_szama" ~ "number of doors",
    x == "ajtok_szama5" ~ "number of doors: 5",
    x == "ajtok_szama4" ~ "number of doors: 4",
    x == "ajtok_szama3" ~ "number of doors: 3",
    x == "klima_fajtaja" ~ "type of climate",
    x == "uzemanyag" ~ "type of fuel",
    x == "hengerurtartalom" ~ "cylinder capacity",
    x == "teljesitmeny" ~ "engine performance",
    x == "henger_elrendezes" ~ "cylinder layout",
    x == "henger_elrendezesV" ~ "cylinder layout: V",
    x == "henger_elrendezesOther" ~ "cylinder layout: Other",
    x == "hajtas" ~ "wheel drive", # TODO
    x == "sebessegvalto_fajtaja" ~ "type of gear",
    x == "muszaki_vizsga_ervenyes" ~ "valid MOT",
    x == "nyari_gumi_meret" ~ "tire size 1",
    x == "szin" ~ "color",
    x == "sajat_tomeg" ~ "own weight",
    x == "teljes_tomeg" ~ "total weight",
    x == "csomagtarto" ~ "trunk",
    x == "nyari_gumi_meret2" ~ "tire size 2",
    x == "nyari_gumi_meret3" ~ "tire size 3",
    x == "sebessegvalto_fokozatszam" ~ "number of breaks on the gear",
    x == "id" ~ x,
    x == "dontheto_utasulesek" ~ x,
    x == "pluss_karpit" ~ "plush upholstery",
    x == "ulesmagassag_allitas" ~ x,
    x == "kozepso_kartamasz" ~ x,
    x == "multifunkcios_kormanykerek" ~ x,
    x == "velur_karpit" ~ x,
    x == "fuggonylegzsak" ~ "curtain airbag",
    x == "isofix_rendszer" ~ x,
    x == "kikapcsolhato_legzsak" ~ x,
    x == "oldallegzsak" ~ x,
    x == "utasoldali_legzsak" ~ x,
    x == "vezetooldali_legzsak" ~ x,
    x == "hatso_fejtamlak" ~ x,
    x == "allithato_kormany" ~ "adjustable wheel",
    x == "fedelzeti_komputer" ~ x,
    x == "tavolsagi_fenyszoro_asszisztens" ~ x,
    x == "abs_blokkolasgatlo" ~ x,
    x == "esp_menetstabilizator" ~ x,
    x == "ebd_ebv_elektronikus_fekero_eloszto" ~ x,
    x == "savtarto_rendszer" ~ "lane departure warning",
    x == "tabla_felismero_funkcio" ~ "traffic-sign recognition",
    x == "guminyomas_ellenorzo_rendszer" ~ x,
    x == "inditasgatlo_immobiliser" ~ x,
    x == "savvalto_asszisztens" ~ x,
    x == "visszagurulas_gatlo" ~ x,
    x == "lejtmenet_asszisztens" ~ x,
    x == "fekasszisztens" ~ "brake assist",
    x == "centralzar" ~ x,
    x == "riaszto" ~ "car alarm",
    x == "szervokormany" ~ "servo-control",
    x == "tempomat" ~ x,
    x == "sebessegfuggo_szervokormany" ~ x,
    x == "elektromosan_behajthato_kulso_tukrok" ~ "electric mirrors",
    x == "fenyszoro_magassagallitas" ~ x,
    x == "elektromos_ablak_elol" ~ x,
    x == "elektromos_ablak_hatul" ~ x,
    x == "elektromos_tukor" ~ x,
    x == "futheto_tukor" ~ "heated mirror",
    x == "konnyufem_felni" ~ x,
    x == "szinezett_uveg" ~ x,
    x == "defektjavito_keszlet" ~ "puncture repair kit",
    x == "bluetooth_os_kihangosito" ~ x,
    x == "i_phone_i_pod_csatlakozo" ~ x,
    x == "usb_csatlakozo" ~ x,
    x == "tolatokamera_bemenet" ~ x,
    x == "erintokijelzo" ~ x,
    x == "hangszoro" ~ x,
    x == "autobeszamitas_lehetseges" ~ "car set off",
    x == "elso_forgalomba_helyezes_magyarorszagon" ~ "authorization in Hungary",
    x == "garancialis" ~ "warranty",
    x == "rendelheto" ~ "can be ordered",
    x == "torzskonyv" ~ x,
    x == "vezetett_szervizkonyv" ~ x,
    x == "allithato_hatso_ulesek" ~ "adjustable back seat",
    x == "tolatoradar" ~ x,
    x == "asr_kiporgesgatlo" ~ x,
    x == "kodlampa" ~ x,
    x == "esoszenzor" ~ x,
    x == "led_fenyszoro" ~ "led headlight",
    x == "aux_csatlakozo" ~ x,
    x == "mp3_lejatszas" ~ "mp3 player",
    x == "mp4_lejatszas" ~ "mp4 player",
    x == "hifi" ~ x,
    x == "futheto_elso_ules" ~ x,
    x == "radio" ~ x,
    x == "bor_belso" ~ x,
    x == "faberakas" ~ x,
    x == "automatikusan_sotetedo_belso_tukor" ~ x,
    x == "elektromos_ulesallitas_vezetooldal" ~ x,
    x == "elektromos_ulesallitas_utasoldal" ~ x,
    x == "reszecskeszuro" ~ "particulate filter",
    x == "kulcsnelkuli_inditas" ~ "keyless engine start",
    x == "automatikusan_sotetedo_kulso_tukor" ~ x,
    x == "gps_navigacio" ~ x,
    x == "dvb_t_tuner" ~ x,
    x == "azonnal_elviheto" ~ "can be taken immediately",
    x == "hutheto_kesztyutarto" ~ x,
    x == "allithato_combtamasz" ~ x,
    x == "derektamasz" ~ x,
    x == "bor_szovet_huzat" ~ x,
    x == "hatso_oldal_legzsak" ~ x,
    x == "kormanyvalto" ~ x,
    x == "ard_automatikus_tavolsagtarto" ~ x,
    x == "futoszalas_szelvedo" ~ "heated windshield",
    x == "kanyarkoveto_fenyszoro" ~ x,
    x == "kihangosito" ~ "speakerphone",
    x == "x8_hangszoro" ~ "8 speakers",
    x == "tolatokamera" ~ "reversing camera",
    x == "tavolsagtarto_tempomat" ~ x,
    x == "potkerek" ~ x,
    x == "bemutato_jarmu" ~ "exhibited car",
    x == "x4_hangszoro" ~ x,
    x == "futheto_kormany" ~ x,
    x == "holtter_figyelo_rendszer" ~ x,
    x == "fenyszoromoso" ~ x,
    x == "keramia_fektarcsak" ~ x,
    x == "toloajto" ~ x,
    x == "terdlegzsak" ~ x,
    x == "kiegeszito_fenyszoro" ~ "addtitional headlight",
    x == "memoriakartya_olvaso" ~ x,
    x == "erosito_kimenet" ~ "amplifier output",
    x == "mikrofon_bemenet" ~ x,
    x == "erosito" ~ x,
    x == "gyari_erosito" ~ x,
    x == "x2_din" ~ x,
    x == "sportulesek" ~ x,
    x == "krom_felni" ~ x,
    x == "csomag_rogzito" ~ x,
    x == "sportfutomu" ~ "sports suspension",
    x == "memorias_vezetoules" ~ x,
    x == "ads_adaptiv_lengescsillapito" ~ x,
    x == "allithato_felfuggesztes" ~ x,
    x == "elektromos_csomagterajto_mozgatas" ~ x,
    x == "toloteto_napfenyteto" ~ x,
    x == "toloteto_elektromos" ~ x,
    x == "sebessegvalto_zar" ~ x,
    x == "vonohorog" ~ x,
    x == "elso_tulajdonostol" ~ x,
    x == "keveset_futott" ~ "low milage",
    x == "nem_dohanyzo" ~ x,
    x == "rendszeresen_karbantartott" ~ x,
    x == "dokumentumok" ~ "documents",
    x == "x230v_csatlakozo_hatul" ~ x,
    x == "x360_fokos_kamerarendszer" ~ x,
    x == "elso_hatso_parkoloradar" ~ "parking radar",
    x == "start_stop_motormegallito_rendszer" ~ "start-stop",
    x == "cd_s_autoradio" ~ "CD player",
    x == "garantalt_km_futas" ~ x,
    x == "masodik_tulajdonostol" ~ x,
    x == "garazsban_tartott" ~ "kept in a garage",
    x == "holgy_tulajdonostol" ~ x,
    x == "hangvezerles" ~ x,
    x == "frissen_szervizelt" ~ "freshly serviced",
    x == "cd_tar" ~ x,
    x == "uleshutes_szelloztetes" ~ x,
    x == "xenon_fenyszoro" ~ x,
    x == "edc_elektronikus_lengescsillapitas_vezerles" ~ x,
    x == "masszirozos_ules" ~ x,
    x == "merevlemez" ~ x,
    x == "borkormany" ~ "leather steering wheel",
    x == "digitalis_muszeregyseg" ~ x,
    x == "hud_head_up_display" ~ x,
    x == "kulcsnelkuli_nyitorendszer" ~ "keyless entry",
    x == "elektronikus_rogzitofek" ~ x,
    x == "utkozes_veszelyre_felkeszito_rendszer" ~ x,
    x == "faradtsagerzekelo" ~ x,
    x == "veszfek_asszisztens" ~ x,
    x == "automata_fenyszorokapcsolas" ~ x,
    x == "automata_tavfeny" ~ x,
    x == "menetfeny" ~ x,
    x == "bukolampa" ~ "tilting lamp",
    x == "led_matrix_fenyszoro" ~ x,
    x == "android_auto" ~ "android car",
    x == "apple_car_play" ~ "Apple Car Play",
    x == "kormanyrol_vezerelheto_hifi" ~ x,
    x == "multifunkcionalis_kijelzo" ~ x,
    x == "gps_nyomkoveto" ~ x,
    x == "x7_hangszoro" ~ x,
    x == "mubor_karpit" ~ x,
    x == "futheto_ablakmoso_fuvokak" ~ x,
    x == "elektronikus_futomu_hangolas" ~ x,
    x == "autotelefon" ~ x,
    x == "x10_hangszoro" ~ x,
    x == "dvd" ~ x,
    x == "x12_hangszoro" ~ x,
    x == "ajtoszervo" ~ x,
    x == "defektturo_abroncsok" ~ x,
    x == "allofutes" ~ x,
    x == "eds_elektronikus_differencialzar" ~ "EDS",
    x == "memorias_utasules" ~ x,
    x == "futheto_elso_es_hatso_ulesek" ~ x,
    x == "vezetek_nelkuli_telefontoltes" ~ x,
    x == "motorbeszamitas_lehetseges" ~ x,
    x == "full_extra" ~ x,
    x == "hutheto_kartamasz" ~ x,
    x == "elektromosan_allithato_fejtamlak" ~ x,
    x == "wma_lejatszas" ~ x,
    x == "msr_motorfeknyomatek_szabalyzas" ~ x,
    x == "bi_xenon_fenyszoro" ~ x,
    x == "sperr_differencialmu" ~ x,
    x == "radaros_fekasszisztens" ~ x,
    x == "afa_visszaigenyelheto" ~ "VAT Refund possible",
    x == "gyalogos_legzsak" ~ x,
    x == "automatikus_segelyhivo" ~ "automatic emergency call",
    x == "parkoloasszisztens" ~ x,
    x == "x9_hangszoro" ~ x,
    x == "tv" ~ x,
    x == "automatikus_hengerlekapcsolas" ~ x,
    x == "rablasgatlo" ~ x,
    x == "melynyomo" ~ x,
    x == "vonohorog_elektromosan_kihajthato" ~ x,
    x == "bekanyarodasi_segedfeny" ~ x,
    x == "ejjellato_asszisztens" ~ x,
    x == "garazsajto_taviranyito" ~ x,
    x == "hatso_keresztiranyu_forgalomra_figyelmeztetes" ~ x,
    x == "koccanasgatlo" ~ x,
    x == "alcantara_karpit" ~ x,
    x == "dvb_tuner" ~ x,
    x == "allo_helyzeti_klima" ~ x,
    x == "type2_toltokabel" ~ x,
    x == "otthoni_halozati_tolto" ~ x,
    x == "gesztusvezerles" ~ x,
    x == "hdmi_bemenet" ~ x,
    x == "radios_magno" ~ x,
    x == "fejtamlamonitor" ~ x,
    x == "hud_head_up_display_kiterjesztett_valosag_funkcioval" ~ x,
    x == "wi_fi_hotspot" ~ x,
    x == "x1_din" ~ x,
    x == "x2_hangszoro" ~ x,
    x == "tetocsomagtarto" ~ x,
    x == "taviranyito" ~ "remote control",
    x == "ketoldali_toloajto" ~ x,
    x == "mozgasserult" ~ x,
    x == "x5_hangszoro" ~ x,
    x == "kozepso_legzsak_elol" ~ x,
    x == "fm_transzmitter" ~ x,
    x == "kormanyra_szerelheto_taviranyito" ~ x,
    x == "vonohorog_leveheto_fejjel" ~ x,
    x == "chiptuning" ~ x,
    x == "jobbkormanyos" ~ x,
    x == "analog_tv_tuner" ~ x,
    x == "bukocso" ~ x,
    x == "amerikai_modell" ~ "american model",
    x == "beepitett_gyerekules" ~ x,
    x == "x11_hangszoro" ~ x,
    x == "taxi" ~ x,
    x == "bekanyarodasi_asszisztens" ~ x,
    x == "x4ws_osszkerekkormanyzas" ~ x,
    x == "taviranyitassal_ledontheto_hatso_ulestamla" ~ x,
    x == "tetomonitor" ~ x,
    x == "pot_uzemanyagtartaly" ~ x,
    x == "tetore_szerelheto_kerekpartarto" ~ x,
    x == "vonohorgos_kerekpartarto" ~ x,
    x == "vegig_vezetett_szervizkonyv" ~ x,
    x == "kulcsnelkuli_nyitorendszer_2" ~ x,
    TRUE ~ x
  )
  
  if (as_sentence) {
    out <- str_c(
      str_sub(out, end = 1) %>% 
        str_to_sentence(),
      str_sub(out, start = 2)
    )
  }
  out
}

dummy_df <- function(.data) {
  imap_dfc(.data, ~ {
    if (is.numeric(.x)) {
      return(.x)
    } else {
      pos <- unique(as.character(.x))
      map_dfc(pos, function(p){
        tibble(ifelse(.x == p, 1, 0)) %>% 
          set_names(str_c(.y, p))
      })
    }
  }
  )
}

table_output <- function(x, align = NULL) {
  
  if (knitr::is_latex_output()) {
    
    if (is.null(align)) {
      align <- c("l", "c", "c", "c", "c", "c", "c")
    }
    
    current_label <- knitr::opts_current$get()$label
    
    current_inputs <- tibble(value = read_lines("markdown/table-labels.txt")) %>% 
      mutate(
        name = case_when(
          str_detect(value, "title:") ~ "title",
          str_detect(value, "comment:") ~ "comment",
          TRUE ~ "label",
        ),
        id = ifelse(name == "label", value, NA),
        value = str_remove(value, ".*:"),
        value = str_trim(value)
      ) %>% 
      fill(id) %>% 
      filter(id != "") %>% 
      pivot_wider(values_fill = "") %>% 
      filter(label == current_label) %>% 
      select(title, comment)
    
    title <- current_inputs$title[1]
    
    comment <- current_inputs$comment[1]
    
    table_code <- as.character(knitr::kable(x, format = "latex", align = align)) %>% 
      str_replace("hline", "toprule") %>% 
      str_replace("hline", "midrule") %>% 
      str_replace("end[{]tabular[}]", "botrule\n\\\\end{tabular}") %>% 
      str_remove_all("\\\\hline\\n")
    
    begin_tabular <- str_extract(table_code, "begin[{]tabular.*[}]")
    
    table_code <- table_code %>% 
      str_replace(
        pattern = begin_tabular %>% 
          str_replace_all("[{]", "[{]") %>% 
          str_replace_all("[}]", "[}]") %>% 
          str_replace_all("[|]", "[|]"),
        replacement = begin_tabular %>% 
          str_remove("tabular") %>% 
          str_extract("[{].*[}]") %>% 
          str_remove_all("\\W") %>% 
          str_c("\\begin{tabular}{\\\\textwidth}{@{\\\\extracolsep{\\\\fill}}", ., "@{\\\\extracolsep{\\\\fill}}}")
      )
    
    if (comment != "") {
      table_code <- table_code %>% 
        str_c("\n\\begin{tablenotes}\n\\item Note: ", comment, "\n\\end{tablenotes}\n")
    }
    
    table_code %>% 
      str_replace_all("[{}]tabular[}]", "{tabular*}") %>% 
      str_c("\\begin{table*}[t]\n\\caption{", title, 
            "\\label{tab:",current_label, "}}\n",
            .,
            "\\end{table*}\n"
      ) %>% 
      knitr::asis_output()
  } else {
    x
  }
}


