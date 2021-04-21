# R-Shiny-App für Labormedizinische Estimationen
library(shiny)
# Formeln
CG <- function(Krea, Alter, Gewicht, female = FALSE, Konv = TRUE){
  if (Konv == TRUE){
    if (female == FALSE){
      x <- (((140-Alter)*Gewicht)/(Krea*72))
    } else {
      x <- (((140-Alter)*Gewicht)/(Krea*72))*0.85
    }
  } else {
    if (female == FALSE){
      x <- (((140-Alter)*Gewicht)/((Krea/88.4)*72))
    } else {
      x <- (((140-Alter)*Gewicht)/((Krea/88.4)*72))*0.85
    }
  }
  return(round(x,0))
}
MDRD <- function(Krea, Alter, female = FALSE, Konv = TRUE){
  if (Konv == TRUE){
    if (female == FALSE){
      x <- 186.3*(Krea^-1.154)*(Alter^-0.203)
    } else {
      x <- 186.3*(Krea^-1.154)*(Alter^-0.203)*0.742
    }
  } else {
    if (female == FALSE){
      x <- 186.3*((Krea/88.4)^-1.154)*(Alter^-0.203)
    } else {
      x <- 186.3*((Krea/88.4)^-1.154)*(Alter^-0.203)*0.742
    }
  }
  return(round(x,0))
}
CKD.EPI <- function(Krea, Alter, female = FALSE, Konv = TRUE){
  if (Konv == TRUE){
    if (female == FALSE){
      if (Krea <= 0.9){
        x <- 141*((Krea/0.9)^-0.411)*(0.993^Alter)
      } else {
        x <- 141*((Krea/0.9)^-1.209)*(0.993^Alter)
      }
    } else {
      if (Krea <= 0.7){
        x <- 144*((Krea/0.7)^-0.329)*(0.993^Alter)
      } else {
        x <- 144*((Krea/0.7)^-1.209)*(0.993^Alter)
      }
    }
  } else {
    if (female == FALSE){
      if (Krea <= 80){
        x <- 141*(((Krea/88.4)/0.9)^-0.411)*(0.993^Alter)
      } else {
        x <- 141*(((Krea/88.4)/0.9)^-1.209)*(0.993^Alter)
      }
    } else {
      if (Krea <= 62){
        x <- 144*(((Krea/88.4)/0.7)^-0.329)*(0.993^Alter)
      } else {
        x <- 144*(((Krea/88.4)/0.7)^-1.209)*(0.993^Alter)
      }
    }
  }
  return(round(x,2))
}
BIS1 <- function(Krea, Alter, female = FALSE, Konv = TRUE){
  if (Konv == TRUE){
    if (female == FALSE){
      x <- 3736*(Krea^-0.87)*(Alter^-0.95)
    } else {
      x <- 3736*(Krea^-0.87)*(Alter^-0.95)*0.82
    }
  } else {
    if (female == FALSE){
      x <- 3736*((Krea/88.4)^-0.87)*(Alter^-0.95)
    } else {
      x <- 3736*((Krea/88.4)^-0.87)*(Alter^-0.95)*0.82
    }
  }
  return(round(x,0))
}
# User-Interface
ui <- fluidPage(
  navbarPage(title = "Adler Medical Data Science Web Helper",
             # zlog-Transformation
             tabPanel("Laborvergleich",
                      # Intro Text
                      "Hoffmann et al. haben 2017 (J Lab Med 2017; 41: 23 - 32) mit dem", tags$strong("zlog-Wert"), "eine Methode zur",
                      tags$strong("Standardisierung von Laborwerten"), "sowie der", tags$strong("Übertragung von Messergebnissen
                      zwischen Laboren"), "vorgestellt.",
                      tags$br(),
                      tags$br(),
                      "Mit dieser WebApp können Sie die zlog-Werte Ihrer Messergebnisse berechnen, sowie Ihr Messergebnis ins
                      Verhältnis zu den Referenzintervallgrenzen eines anderen Labores setzen.",
                      tags$br(),
                      tags$br(),
                      "Die voreingestellten Werte zeigen ein Beispiel für eine Messung des Vitamin-B12-Spiegels.",
                      tags$br(),
                      tags$br(),
                      # Inputs
                      fluidRow(
                        column(2, numericInput("UG1", "Untere RI-Grenze Labor 1:", 197)),
                        column(2, numericInput("UG2", "Untere RI-Grenze Labor 2:", 145), offset = 0.5),
                      ),
                      fluidRow(
                        column(2, numericInput("OG1", "Obere RI-Grenze Labor 1:", 771)),
                        column(2, numericInput("OG2", "Obere RI-Grenze Labor 2:", 569), offset = 0.5),
                      ),
                      fluidRow(
                        column(2, numericInput("Wert", "Messwert:", 424)),
                      # Outputs
                        column(2, tags$strong("Messwert umgerechnet:"), verbatimTextOutput("Wertneu"), offset = 0.5)
                      ),
                      tags$strong("zlog-Wert:"),
                      fluidRow(
                        column(1, verbatimTextOutput("zlog")),
                      ),
                      tags$br(),
                      "Made by Jakob Adler"
                      ),
             # eGFR-Vergleich
             tabPanel("eGFR",
                      # Intro Text
                      "Ziel dieser WebApp ist der", tags$strong("Vergleich"), "der estimierten GFR-Werte je nach genutzter
                      Formel sowie der Berechnung des", tags$strong("tatsächlichen Intervalles"), "in dem die", tags$em("wahre"),
                      "GFR zu erwarten ist.",
                      tags$br(),
                      tags$br(),
                      # Inputs
                      fluidRow(
                        column(2, numericInput("Alter", "Alter des Patienten in Jahren:", 70)),
                        column(2, numericInput("Krea", "Kreatinin in µmol/l:", 100), offset = 0.5),
                        column(1, radioButtons("Sex", "Geschlecht:", c("Mann" = "M", "Frau" = "Fr")), offset = 0.5),
                        column(2, numericInput("Gew", "Gewicht in Kg:", 90), offset = 0.5)
                      ),
                      # Outputs
                      fluidRow(
                        column(2, tags$strong("eGFR nach Cockcroft-Gault:"), verbatimTextOutput("CG")),
                        column(2, tags$strong("eGFR nach MDRD:"), verbatimTextOutput("MDRD"), offset = 0.5),
                        column(2, tags$strong("eGFR nach CKD-EPI (KDIGO):"), verbatimTextOutput("CKD"), offset = 0.5),
                        column(3, tags$strong("eGFR nach Berlin-Initative-Studie-1 (BIS-1, kreatininbasiert):"), verbatimTextOutput("BIS1"), offset = 0.5)
                      ),
                      tags$br(),
                      "Die Präzision der eGFR-Formeln wird mit dem", tags$strong("P30-Wert"), "angegeben.",
                      "Dieser Wert gibt an, wieviel Prozent der estimierten eGFR-Werte mit maximal +30% bzw. -30% von
                      der", tags$em("wahren"), "GFR abweichen.",
                      tags$br(),
                      "Die meisten Formeln besitzen einen P30-Wert von über 90%.",
                      "Geht man von einer Abweichung von +/- 30% aus, so liegt die", tags$em("wahre"), "eGFR in den folgenden Bereichen.",
                      tags$br(),
                      tags$br(),
                      fluidRow(
                        column(2, tags$strong("eGFR-Bereich Cockcroft-Gault:"), verbatimTextOutput("CG.Ber")),
                        column(2, tags$strong("eGFR-Bereich MDRD:"), verbatimTextOutput("MDRD.Ber"), offset = 0.5),
                        column(2, tags$strong("eGFR-Bereich CKD-EPI:"), verbatimTextOutput("CKD.Ber"), offset = 0.5),
                        column(2, tags$strong("eGFR-Bereich BIS-1 (kreatininbasiert):"), verbatimTextOutput("BIS.Ber"), offset = 0.5)
                      ),
                      tags$br(),
                      "Made by Jakob Adler"
                      ),
             # kinetische eGFR
             tabPanel("kinetische eGFR (ANV/AKI)",
                      # Intro Text
                      "Zu", tags$strong("Beginn"), "eines akuten Nierenversagens wird die GFR", tags$strong("meist überschätzt"),
                      "sodass es bei eGFR-gesteuerter Medikamentendosierung zu", tags$strong("Überdosierungen"), "kommt.",
                      tags$br(),
                      "Im Falle einer", tags$strong("Restitutio"), "der Nierenfunktion", tags$strong("steigt die eGFR nur
                      unzureichend schnell"), "an, sodass es eher zu", tags$strong("Unterdosierungen"), "kommt.",
                      tags$br(),
                      "Um die kinetische Komponente der GFR-Veränderung im Rahmen eines ANV zu erfassen,
                      hat Chen 2013 eine Formel für die kinetische GFR eingefuehrt (J Am Soc Nephrol 2013; 24: 877 - 888).",
                      tags$br(),
                      "Diese Formel ist aber für den klinischen Alltag aufgrund ihrer Komplexität leider nicht
                      zielführend einzusetzen.",
                      tags$br(),
                      tags$br(),
                      "Hierfür wurde von Keller et al. eine vereinfachte Zweipunkt-Formel vorgeschlagen (Trillium
                      Diagnostik 2020; 18: 44 - 47)",
                      tags$br(),
                      tags$br(),
                      "Mit der hier vorgestellten WebApp kann diese kinetische GFR berechnet werden.",
                      tags$br(),
                      tags$br(),
                      # Inputs
                      fluidRow(
                        column(2, numericInput("GFR1", "1. GFR-Messung in ml/min:", 100)),
                        column(2, numericInput("GFR2", "2. GFR-Messung in ml/min:", 50), offset = 0.5),
                        column(8, numericInput("Zeit", "Zeitdifferenz der Messungen in Stunden:", 48), offset = 0.5)
                      ),
                      # Outputs
                      tags$strong("Korrigierte kinetische Zweipunkt-GFR des 2. Messwertes:"),
                      fluidRow(
                        column(4, verbatimTextOutput("GFRkin"))
                      ),
                      "Made by Jakob Adler"
                      ),
             # NT-proBNP Korrektur
             tabPanel("NT-proBNP-Korrektur",
                      # Intro Text
                      "Luchner et al. haben 2010 (Clin Chem Lab Med 2010; 48: 121 - 128)
                      eine Formel zur", tags$strong("eGFR-basierten Korrektur von NT-proBNP Werten vorgestellt."),
                      tags$br(),
                      tags$br(),
                      "Aufgrund des starken Einflusses der Nierenfunktion auf die Höhe des
                      NT-proBNP-Wertes empfiehlt sich eine eGFR-basierte Korrektur des gemessenen NT-proBNPs.",
                      tags$br(),
                      tags$br(),
                      tags$strong("Anmerkung:"), "Eine Korrektur macht aufgrund der zugrundeliegenden
                      Exponentialfunktion erst", tags$strong("ab einer eGFR von < 75 ml/min Sinn."),
                      "Verwendete eGFR-Formel:", tags$strong("Cockcroft-Gault."),
                      tags$br(),
                      tags$br(),
                      # Inputs
                      fluidRow(
                        column(2, numericInput("NTist", "NT-proBNP in pg/ml:", 1000, 100, 30000)),
                        column(2, numericInput("GFRist", "eGFR in ml/min:", 60, 0, 75), offset = 0.5),
                        column(2, tags$strong("korrigierter NT-proBNP-Wert:"), tags$br(),
                               verbatimTextOutput("NTsoll"), offset = 0.5)
                      ),
                      tags$br(),
                      "Made by Jakob Adler"
                      ),
             # LDL-Korrektur
             tabPanel("LDL-Korrektur",
                      # Intro Text
                      "Die Messung als auch die Berechnung des LDL-Cholesterins enthält die Cholesterin-
                      Fraktion des Lipoprotein A (Lp(a)).",
                      tags$br(),
                      "So kommt es bei Patienten mit", tags$strong("hohen Lp(a)-Spiegeln"),"zu einer",
                      tags$strong("Überschätzung des LDL."),
                      tags$br(),
                      "Zur Korrektur dieses Effektes haben Langlois et al. die Lp(a)-basierte LDL-Korrektur publiziert
                      (Clin Chem Lab Med 2020; 58: 496 - 517).",
                      tags$br(),
                      tags$br(),
                      "Mit dieser WebApp können Sie das Lp(a)-korrigierte LDL berechnen.",
                      # Umrechnung
                      tags$br(),
                      tags$br(),
                      tags$strong("Umrechnung des Lp(a):"),
                      tags$br(),
                      tags$br(),
                      fluidRow(
                        column(2, numericInput("Lpa.nmol", "Lp(a) in nmol/l:", 100.00)),
                        column(2, tags$strong("Lp(a) in mg/dl:"), tags$br(), verbatimTextOutput("Lpa.mgdl"), offset = 0.5)
                      ),
                      tags$br(),
                      "Genau genommen ist für Lp(a) die Umrechnung von nmol/l in mg/dl und gegenläufig fachlich nicht
                      richtig. Mol spiegelt die Anzahl von Teilchen wieder, während Gramm das Gewicht angibt.",
                      tags$br(),
                      "Aufgrund der Physiologie des Lp(a) besteht keine lineare Beziehung der Messung in mg/dl bzw. in nmol/l",
                      tags$br(),
                      tags$br(),
                      # Inputs
                      fluidRow(
                        column(2, numericInput("LDL", "LDL in mmol/l:", 6.6)),
                        column(2, numericInput("Lpa", "Lp(a) in mg/dl:", 42.0), offset = 0.5),
                      # Output
                        column(2, tags$strong("Lp(a)-korrigiertes LDL in mmol/l:"), tags$br(), verbatimTextOutput("LDLkor"))
                      ),
                      tags$br(),
                      "Made by Jakob Adler"
                      ),
             # Natrium-Korrektur
             tabPanel("Pseudohyponatriämie",
                      # Intro Text
                      "Hohe Konzentrationen von",tags$strong("Triglyceriden, Glucose sowie Gesamtprotein"),"können zu einer",
                      tags$strong("falsch niedrigen Messung des Serum-Natriums"),"führen.",
                      tags$br(),
                      tags$br(),
                      "Cho et al (J Lab Med 2020; 44: 17 - 22) haben daraufhin zwei Korrekturformeln entwickelt.",
                      tags$br(),
                      tags$br(),
                      "Mit dieser WebApp können Sie die Natriumkonzentration bei Hyperglykämie sowie bei Hypertriglyceridämie korrigieren.",
                      tags$br(),
                      tags$br(),
                      # Inputs 1
                      fluidRow(
                        column(2, numericInput("GlucNa", "Glucose in mmol/l:", 10.5)),
                        column(2, numericInput("Naist", "Natrium in mmol/l:", 125), offset = 0.5),
                      # Output 1
                        column(3, tags$strong("korrigierte Natriumkonzentration in mmol/l:"), tags$br(), verbatimTextOutput("NaKor1"))
                      ),
                      tags$br(),
                      # Inputs 2
                      fluidRow(
                        column(2, numericInput("TriNa", "Triglyceride in mmol/l:", 1.7)),
                        column(2, numericInput("ProtNa", "Gesamtprotein in g/l:", 70), offset = 0.5),
                        column(2, numericInput("Naist2", "Natrium in mmol/l:", 125), offset = 0.5),
                      # Output2
                        column(3, tags$strong("korrigierte Natriumkonzentration in mmol/l:"), tags$br(), verbatimTextOutput("NaKor2"))
                      ),
                      tags$br(),
                      "Made by Jakob Adler"
                      ),
             # TRP
             tabPanel("tubuläre Phosphatreabsorption (TRP)",
                      # Intro Text
                      "Hier können Sie die", tags$strong("tubuläre Phosphatreabsorption (TRP)"), "sowie deren", tags$strong("Korrektur anhand der GFR (TmP/GFR)"), "berechnen.",
                      tags$br(),
                      tags$br(),
                      "Grundlage dieser Berechnung ist die Arbeit von Bart, Jones und Payne von 2000 (Ann Clin Biochem 2000; 37: 79-81).",
                      tags$br(),
                      tags$br(),
                      fluidRow(
                        column(2, numericInput("P.Serum", "Phosphat im Serum in mmol/l:", 0.88)),
                        column(2, numericInput("Krea.Serum", "Kreatinin im Serum in µmol/l:", 75)),
                        column(2, numericInput("P.Urin", "Phosphat im Urin in mmol/l:", 30.93)),
                        column(2, numericInput("Krea.Urin", "Kreatinin im Urin in mmol/l:", 10.24))
                      ),
                      fluidRow(
                        column(3, tags$strong("tubuläre Reabsorption von Phosphat (TRP):"), tags$br(), verbatimTextOutput("PTub")),
                        column(3, tags$strong("tubuläre Phosphatreabsorption mit eGFR-Korrektur (TmP/GFR):"), tags$br(), verbatimTextOutput("PTubGFR"))
                      ),
                      tags$br(),
                      tags$strong("Interpretation:"),
                      tags$br(),
                      tags$br(),
                      "Normbereiche TmP/GFR Männer:",
                      tags$br(),
                      "0,90 - 1,35",
                      tags$br(),
                      tags$br(),
                      "Normbereiche TmP/GFR Frauen:",
                      tags$br(),
                      "0,88 - 1,42",
                      tags$br(),
                      tags$br(),
                      "Made by Jakob Adler"
                      ),
             # QT-Zeit-Korrektur
             tabPanel("QT-Zeit-Korrektur",
                      "Mit dieser WebApp können Sie die QT-Zeit nach der Herzfrequenz korrigieren.",
                      tags$br(),
                      tags$br(),
                      # Inputs
                      fluidRow(
                        column(2, numericInput("QT", "QT-Zeit in ms:", 180)),
                        column(2, numericInput("HR", "Herzfrequenz in Schläge/Minute:", 100), offset = 0.5),
                      # Output
                        column(2, tags$strong("korrigierte QT-Zeit in ms:"), tags$br(), verbatimTextOutput("QTKor"))
                      ),
                      tags$br(),
                      "Made by Jakob Adler"
                      ),
             # CHA2DS2-VAsc-Score
             tabPanel("CHA2DS2-VAsc-Score",
                      "Der CHA2DS2-VAsc-Score wird zur Einschätzung des", tags$strong("Schlaganfallrisikos"), "genutzt",
                      tags$br(),
                      tags$br(),
                      # Inputs
                      fluidRow(
                        column(2, radioButtons("HI", "Herzinsuffizienz (C):", c("Ja" = "HIja", "Nein" = "HInein"), selected = "HInein")),
                        column(2, radioButtons("AHT", "Hypertonie (H):", c("Ja" = "AHTja", "Nein" = "AHTnein"), selected = "AHTnein"), offset = 0.5),
                        column(2, radioButtons("Alter2", "Alter (A2 & A):", c("75 Jahre oder älter" = "Alter2", "65 - 75 Jahre" = "Alter2", "< 65 Jahre" = "Alter0")), offset = 0.5),
                        column(2, radioButtons("Diab", "Diabetes mellitus (D):", c("Ja" = "Diabja", "Nein" = "Diabnein"), selected = "Diabnein"), offset = 0.5)
                      ),
                      tags$br(),
                      fluidRow(
                        column(2, radioButtons("Stroke", "TIA oder Schlaganfall (S2):", c("Ja" = "Strokeja", "Nein" = "Strokenein"), selected = "Strokenein")),
                        column(2, radioButtons("Frau", "Geschlecht (Sc):", c("Frau" = "Frauja", "Mann" = "Fraunein")), offset = 0.5),
                        column(2, radioButtons("Vasc", "MI, KHK, pAVK, Thrombose (V):", c("Ja" = "Vascja", "Nein" = "Vascnein"), selected = "Vascnein"), offset = 0.5)
                      ),
                      tags$br(),
                      # Outputs
                      fluidRow(
                        column(2, tags$strong("CHA2DS2-VAsc-Score:"), tags$br(), verbatimTextOutput("Chad")),
                      ),
                      tags$br(),
                      "Made by Jakob Adler"
                      ),
             # Wells-Score
             tabPanel("Wells-Score",
                      "Mit dieser WebAPp können Sie den", tags$strong("Wells-Score"), "für eine", tags$strong("tiefe Venenthrombose"), "sowie eine",
                      tags$strong("Lungenarterienembolie"), "berechnen.",
                      tags$br(),
                      tags$br(),
                      tags$strong("Wells-Score zur Bestimmung der klinischen Wahrscheinlichkeit einer tiefen Beinvenenthrombose (TVT):"),
                      # Inputs 1
                      fluidRow(
                        column(2, radioButtons("Mal", "Malignom:", c("Ja" = "Malja", "Nein" = "Malnein"), selected = "Malnein")),
                        column(2, radioButtons("Imm", "Immobilisation/Lähmung:", c("Ja" = "Immja", "Nein" = "Immnein"), selected = "Immnein"), offset = 0.5),
                        column(2, radioButtons("Bett", "Bettlägerigkeit (>3d)/große Operation:", c("Ja" = "Bettja", "Nein" = "Bettnein"), selected = "Bettnein"), offset = 0.5),
                        column(2, radioButtons("Pain", "Schmerz/Verhärtung der Venen:", c("Ja" = "Painja", "Nein" = "Painnein"), selected = "Painnein"), offset = 0.5),
                        column(2, radioButtons("Schwell", "Beinschwellung:", c("Ja" = "Schwellja", "Nein" = "Schwellnein"), selected = "Schwellnein"), offset = 0.5)
                      ),
                      fluidRow(
                        column(2, radioButtons("Umfang", "US-Umfagsdifferenz > 3cm:", c("Ja" = "Umfangja", "Nein" = "Umfangnein"), selected = "Umfangnein")),
                        column(2, radioButtons("Ödem", "Eindrückbares Ödem:", c("Ja" = "Ödemja", "Nein" = "Ödemnein"), selected = "Ödemnein"), offset = 0.5),
                        column(2, radioButtons("Koll", "Sichtbare Kollateralen:", c("Ja" = "Kollja", "Nein" = "Kollnein"), selected = "Kollnein"), offset = 0.5),
                        column(2, radioButtons("TVT", "dokumentierte TVT-Vorgeschichte:", c("Ja" = "TVTja", "Nein" = "TVTnein"), selected = "TVTnein"), offset = 0.5),
                        column(2, radioButtons("Alt", "Alternative Diagnose:", c("Ja" = "Altja", "Nein" = "Altnein"), selected = "Altnein"), offset = 0.5)
                      ),
                      # Output 1
                      fluidRow(
                        column(2, tags$strong("Wells-Score:"), tags$br(), verbatimTextOutput("Wells1"))
                      ),
                      tags$strong("Aus einem Wells-Score von 2 oder mehr Punkten ergibt sich ein hohes TVT-Risiko!"),
                      tags$br(),
                      tags$br(),
                      tags$strong("Wells-Score zur Bestimmung der klinischen Wahrscheinlichkeit einer Lungenarterienembolie (LAE):"),
                      # Inputs 2
                      fluidRow(
                        column(2, radioButtons("TVTSymp", "Symptome einer TVT:", c("Ja" = "TVTSympja", "Nein" = "TVTSympnein"), selected = "TVTSympnein")),
                        column(2, radioButtons("LAE", "LAE wahrscheinlich:", c("Ja" = "LAEja", "Nein" = "LAEnein"), selected = "LAEnein"), offset = 0.5),
                        column(2, radioButtons("HF", "Herzfrequenz > 100/min:", c("Ja" = "HFja", "Nein" = "HFnein"), selected = "HFnein"), offset = 0.5),
                        column(2, radioButtons("OP", "Operation/Immobilisation (letzte 4 Wochen):", c("Ja" = "OPja", "Nein" = "OPnein"), selected = "OPnein"), offset = 0.5)
                      ),
                      fluidRow(
                        column(2, radioButtons("TVTalt", "frühere TVT oder LAE:", c("Ja" = "TVTaltja", "Nein" = "TVTaltnein"), selected = "TVTaltnein")),
                        column(2, radioButtons("Häm", "Hämoptysen:", c("Ja" = "Hämja", "Nein" = "Hämnein"), selected = "Hämnein"), offset = 0.5),
                        column(2, radioButtons("Tumor", "Tumor/Tumortherapie (letzte 6  Monate):", c("Ja" = "Tumorja", "Nein" = "Tumornein"), selected = "Tumornein"), offset = 0.5)
                      ),
                      # Output 2
                      fluidRow(
                        column(2, tags$strong("Wells-Score:"), tags$br(), verbatimTextOutput("Wells2"))
                      ),
                      tags$strong("Bewertung:"),
                      tags$br(),
                      "Geringe Wahrscheinlichkeit einer LAE: < 2 Punkte", tags$br(),
                      "Mittlere Wahrscheinlichkeit einer LAE: 2 - 6 Punkte", tags$br(),
                      "Hohe Wahrscheinlichkeit einer LAE: > 6 Punkte", tags$br(),
                      tags$br(),
                      "Made by Jakob Adler"
                      ),
             # HAS-BLED-Score
             tabPanel("HAS-BLED-Score",
                      "Mit dieser Webapp können Sie den", tags$strong("HAS-BLED-Score"), "zur Beuteilung des", tags$strong("Blutungsrisikos unter Antokoagulantien bei Vorhofflimmern"),
                      "berechnen.",
                      tags$br(),
                      tags$br(),
                      # Input
                      fluidRow(
                        column(2, radioButtons("Hyp", "Hypertonie:", c("Ja" = "Hypja", "Nein" = "Hypnein"), selected = "Hypnein")),
                        column(2, radioButtons("Niere", "Nierenfunktions-/Leberfunktionsstörung:", c("Keine" = "Keineja", "Niere" = "Niereja", "Leber" = "Leberja", "Beide" = "Beideja"), selected = "Keineja"), offset = 0.5),
                        column(2, radioButtons("Stroke2", "früherer Schlaganfall:", c("Ja" = "Stroke2ja", "Nein" = "Stroke2nein"), selected = "Stroke2nein"), offset = 0.5),
                        column(2, radioButtons("Blut", "frühere Blutung:", c("Ja" = "Blutja", "Nein" = "Blutnein"), selected = "Blutnein"), offset = 0.5)
                      ),
                      fluidRow(
                        column(2, radioButtons("INR", "INR-EInstellung labil:", c("Ja" = "INRja", "Nein" = "INRnein"), selected = "INRnein")),
                        column(2, radioButtons("Alter3", "Alter > 65 Jahre:", c("Ja" = "Alter3ja", "Nein" = "Alter3nein"), selected = "Alter3nein"), offset = 0.5),
                        column(2, radioButtons("Meds", "Medikamente/Alkohol:", c("Keine" = "Keine2ja", "Medikamente" = "Medsja", "Alkohol" = "Alkja", "Beides" = "Beide2ja"), selected = "Keine2ja"), offset = 0.5)
                      ),
                      # Output
                      fluidRow(
                        column(2, tags$strong("HAS-BLED-Score:"), tags$br(), verbatimTextOutput("HAS"))
                      ),
                      tags$strong("Bewertung:"),
                      tags$br(),
                      "Bei 3 oder mehr Punkten ist vor einem erhöhten Blutunsrisiko auszugehen.", tags$strong("Indikation prüfen!"), tags$br(),
                      tags$br(),
                      "Quelle: Pisters et al, A Novel User-Friendly Score (HAS-BLED) To Asses 1-Year Risk of Major Bleeding in Patients With Atrial Fibrillation, Chest 2010; 138: 1093 - 1100.",
                      tags$br(),
                      tags$br(),
                      "Made by Jakob Adler"
                      ),
             # Child-Pugh-Score
             tabPanel("Child-Pugh-Kriterien",
                      "Mit dieser Webapp können Sie die", tags$strong("Child-Pugh-Stadien"), "anhand der Child-Pugh-Kriterien berechnen.",
                      tags$br(),
                      tags$br(),
                      fluidRow(
                        column(2, radioButtons("Alb", "Albumin in g/l:", c("> 35" = "Alb1", "28 - 35" = "Alb2", "< 28" = "Alb3"), selected = "Alb1")),
                        column(2, radioButtons("Bili", "Bilirubin in µmol/l:", c("< 35 (bei PBC/PSC < 70)" = "Bili1", "35 - 50 (bei PBC/PSC 70 - 170)" = "Bili2", "> 50 (bei PBC/PSC > 170)" = "Bili3"), selected = "Bili1"), offset = 0.5),
                        column(2, radioButtons("Quick", "Quick in %:", c("> 70" = "Quick1", "40 - 70" = "Quick2", "< 40" = "Quick3"), selected = "Quick1"), offset = 0.5)
                      ),
                      fluidRow(
                        column(2, radioButtons("Asz", "Aszites (sonographisch):", c("kein Aszites" = "Asz1", "leicht" = "Asz2", "mittelmäßig" = "Asz3"), selected = "Asz1")),
                        column(2, radioButtons("Enc", "Hepatische Encephalppathie:", c("keine" = "Enc1", "I - II" = "Enc2", "III - IV" = "Enc3"), selected = "Enc1"), offset = 0.5),
                      ),
                      fluidRow(
                        column(2, tags$strong("Child-Pugh-Punktzahl:"), tags$br(), verbatimTextOutput("Child"))
                      ),
                      tags$strong("Bewertung:"),
                      tags$br(),
                      "CHILD A = 5 - 6 Punkte", tags$br(),
                      "CHILD B = 7 - 9 Punkte", tags$br(),
                      "CHILD C = 10 - 15 Punkte", tags$br(),
                      tags$br(),
                      "Made by Jakob Adler"
                      ),
             # GRACE-Score
             tabPanel("GRACE-Score",
                      "Mit dieser Webapp können Sie den", tags$strong("GRACE-Score"), "zur Einschätzung der", tags$strong("Wahrscheinlichkeit des Todes während des Krankhausaufenthaltes"), "berechnen.",
                      tags$br(),
                      tags$br(),
                      # Inputs
                      fluidPage(
                        column(2, radioButtons("Killip", "Killip-Class:", c("I" = "Killip1", "II" = "Killip2", "III" = "Killip3", "IV" = "Killip4"), selected = "Killip1")),
                        column(2, radioButtons("Sys", "Systolischer Blutdruck:", c("< 80" = "Sys1", "80 - 99" = "Sys2", "100 - 119" = "Sys3", "120 - 139" = "Sys4", "140 - 159" = "Sys5",
                                                                                  "160 - 199" = "Sys6", "> 200" = "Sys7"), selected = "Sys1"), offset = 0.5),
                        column(2, radioButtons("HFR", "Herzfrequenz:", c("< 50" = "HFR1", "50 - 69" = "HFR2", "70 - 89" = "HFR3", "90 - 109" = "HFR4", "110 - 149" = "HFR5",
                                                                                  "150 - 199" = "HFR6", "> 200" = "HFR7"), selected = "HFR1"), offset = 0.5),
                        column(2, radioButtons("Alter4", "Alter:", c("< 30" = "Alter41", "30 - 39" = "Alter42", "40 - 49" = "Alter43", "50 - 59" = "Alter44", "60 - 69" = "Alter45",
                                                                         "70 - 79" = "Alter46", "80 - 89" = "Alter47", "> 90" = "Alter48"), selected = "Alter41"), offset = 0.5),
                      ),
                      fluidRow(
                        column(2, radioButtons("CreaGrace", "Kreatinin in µmol/l:", c("< 35" = "CreaGrace1", "35 - 70" = "CreaGrace2", "71 - 105" = "CreaGrace3", "106 - 141" = "CreaGrace4",
                                                                                     "142 - 176" = "CreaGrace5", "177 - 353" = "CreaGrace6", "> 354" = "CreaGrace7"), selected = "CreaGrace1")),
                        column(2, radioButtons("Cardiac", "Herzstillstand bei Ankunft:", c("Ja" = "Cardiac1", "Nein" = "Cardiac2"), selected = "Cardiac2"), offset = 0.5),
                        column(2, radioButtons("ST", "ST-Strecken-Veränderungen:", c("Ja" = "ST1", "Nein" = "ST2"), selected = "ST2"), offset = 0.5),
                        column(2, radioButtons("Trop", "Erhöhte Herzenzyme:", c("Ja" = "Trop1", "Nein" = "Trop2"), selected = "Trop2"), offset = 0.5)
                      ),
                      fluidRow(
                        column(2, tags$strong("GRACE-Score:"), tags$br(), verbatimTextOutput("Grace"))
                      ),
                      tags$strong("Bewertung:"),
                      tags$br(),
                      "Punktzahl -> Wahrscheinlichkeit des Todes", tags$br(),
                      "< 60 -> <0.2%", tags$br(),
                      "60 - 70 -> 0.3%", tags$br(),
                      "70 - 80 -> 0.4%", tags$br(),
                      "80 - 90 -> 0.6%", tags$br(),
                      "90 - 100 -> 0.8%", tags$br(),
                      "100 - 110 -> 1.1%", tags$br(),
                      "110 - 120 -> 1.6%", tags$br(),
                      "120 - 130 -> 2.1%", tags$br(),
                      "130 - 140 -> 2.9%", tags$br(),
                      "140 - 150 -> 3.9%", tags$br(),
                      "150 - 160 -> 5.4%", tags$br(),
                      "160 - 170 -> 7.3%", tags$br(),
                      "170 - 180 -> 9.8%", tags$br(),
                      "180 - 190 -> 13.0%", tags$br(),
                      "190 - 200 -> 18.0%", tags$br(),
                      "200 - 210 -> 23.0%", tags$br(),
                      "210 - 220 -> 29.0%", tags$br(),
                      "220 - 230 -> 36.0%", tags$br(),
                      "230 - 250 -> 44.0%", tags$br(),
                      "> 250 -> > 52.0%", tags$br(),
                      tags$br(),
                      "Quelle: Granger et al, Predictors of Hospital Mortality in the Global Registry of Acute Coronary Events, Arch Intern Med 2003; 163: 2345 - 2353.",
                      tags$br(),
                      tags$br(),
                      "Made by Jakob Adler"
                      )
             )
  )
  
server <- function(input, output){
  # zlog-Transformation
    zlog.Wert <- reactive({
      round((log(input$Wert)-(log(input$UG1)+log(input$OG1))/2)*3.92/(log(input$OG1)-log(input$UG1)), 2)
    })
    output$zlog <- renderPrint({
      zlog.Wert()
    })
    output$Wertneu <- renderPrint({
      round((input$UG2^(0.5-(zlog.Wert()/3.92)) * input$OG2^(0.5+(zlog.Wert()/3.92))), 2)
    })
  # eGFR-Vergleich
    CG <- reactive({
      SEX <- switch(input$Sex, M = 1, Fr = 0.85)
      round((((140-input$Alter)*input$Gew)/((input$Krea/88.4)*72))*SEX, 0)
    })
    MDRD <- reactive({
      SEX2 <- switch(input$Sex, M = 1, Fr = 0.742)
      round(186.3*((input$Krea/88.4)^-1.154)*(input$Alter^-0.203)*SEX2, 0)
    })
    CKD <- reactive({
      SEX3 <- switch(input$Sex, M = 1, Fr = 2)
      CKD.EPI <- function(Crea, Age, sex){
        if (sex == 1){
          if (Crea <= 80){
            x <- 141*(((Crea/88.4)/0.9)^-0.411)*(0.993^Age)
          } else {
            x <- 141*(((Crea/88.4)/0.9)^-1.209)*(0.993^Age)
          }
        } else {
          if (Crea <= 62){
            x <- 144*(((Crea/88.4)/0.7)^-0.329)*(0.993^Age)
          } else {
            x <- 144*(((Crea/88.4)/0.7)^-1.209)*(0.993^Age)
          }
        }
        return(round(x,0))
      }
      CKD.EPI(input$Krea, input$Alter, SEX3)
    })
    BIS <- reactive({
      SEX4 <- switch(input$Sex, M = 1, Fr = 0.82)
      round(3736*((input$Krea/88.4)^-0.87)*(input$Alter^-0.95)*SEX4, 0)
    })
    output$CG <- renderPrint({
      CG()
    })
    output$MDRD <- renderPrint({
      MDRD()
    })
    output$CKD <- renderPrint({
      CKD()
    })
    output$BIS1 <- renderPrint({
      BIS()
    })
    output$CG.Ber <- renderPrint({
      eGFR.Bereich <- function(eGFR){
        # eGFR in ml/min
        # P30 als Dezimalzahl (Bsp.: 90% = 0.9)
        OG <- (eGFR*100)/70
        UG <- (eGFR*100)/130
        Range.eGFR <- matrix(nrow = 1, ncol = 2)
        a <- c("untere Grenze", "obere Grenze")
        colnames(Range.eGFR) <- a
        Range.eGFR[1,1] <- UG
        Range.eGFR[1,2] <- OG
        return(round(Range.eGFR, 0))
      }
      eGFR.Bereich(CG())
    })
    output$MDRD.Ber <- renderPrint({
      eGFR.Bereich <- function(eGFR){
        # eGFR in ml/min
        # P30 als Dezimalzahl (Bsp.: 90% = 0.9)
        OG <- (eGFR*100)/70
        UG <- (eGFR*100)/130
        Range.eGFR <- matrix(nrow = 1, ncol = 2)
        a <- c("untere Grenze", "obere Grenze")
        colnames(Range.eGFR) <- a
        Range.eGFR[1,1] <- UG
        Range.eGFR[1,2] <- OG
        return(round(Range.eGFR, 0))
      }
      eGFR.Bereich(MDRD())
    })
    output$CKD.Ber <- renderPrint({
      eGFR.Bereich <- function(eGFR){
        # eGFR in ml/min
        # P30 als Dezimalzahl (Bsp.: 90% = 0.9)
        OG <- (eGFR*100)/70
        UG <- (eGFR*100)/130
        Range.eGFR <- matrix(nrow = 1, ncol = 2)
        a <- c("untere Grenze", "obere Grenze")
        colnames(Range.eGFR) <- a
        Range.eGFR[1,1] <- UG
        Range.eGFR[1,2] <- OG
        return(round(Range.eGFR, 0))
      }
      eGFR.Bereich(CKD())
    })
    output$BIS.Ber <- renderPrint({
      eGFR.Bereich <- function(eGFR){
        # eGFR in ml/min
        # P30 als Dezimalzahl (Bsp.: 90% = 0.9)
        OG <- (eGFR*100)/70
        UG <- (eGFR*100)/130
        Range.eGFR <- matrix(nrow = 1, ncol = 2)
        a <- c("untere Grenze", "obere Grenze")
        colnames(Range.eGFR) <- a
        Range.eGFR[1,1] <- UG
        Range.eGFR[1,2] <- OG
        return(round(Range.eGFR, 0))
      }
      eGFR.Bereich(BIS())
    })
  # kinetische GFR
    output$GFRkin <- renderPrint({
      GFR.delta <- input$GFR1 - input$GFR2
      input$GFR2 - ((24*GFR.delta)/input$Zeit)
    })
  # NTpro-BNP Korrektur Rechnung
    output$NTsoll <- renderPrint({
    round(input$NTist/((exp(1))^(1.892-(0.025*input$GFRist))), 2)
    })
  # LDL-Korrektur
    output$Lpa.mgdl <- renderPrint({
      round(input$Lpa.nmol*0.4167, 0)
    })
    output$LDLkor <- renderPrint({
      round(input$LDL - (input$Lpa*0.0078), 1)
    })
  # Na-Korrektur nach Glucose
    output$NaKor1 <- renderPrint({
      Natrium <- input$Naist
      Glucose <- input$GlucNa*18.02
      round(Natrium + 1.6*(Glucose-100)/100, 0)
    })
  # Na-Korrektur nach Triglyceriden und Protein
    output$NaKor2 <- renderPrint({
      Natrium2 <- input$Naist2
      Protein <- input$ProtNa/10
      Triglyceride <- input$TriNa/0.0114
      round((Natrium2*0.93)/(0.991-(0.001*Triglyceride)-(0.007*Protein)),0)
    })
  # TRP
    # tubuläre Phospatreabsorption normal
    output$PTub <- renderPrint({
      PSe <- input$P.Serum
      KSe <- input$Krea.Serum
      PUr <- input$P.Urin
      KUr <- input$Krea.Urin
      TRP <- 1-((PUr*KSe)/(PSe*(KUr*1000)))
      round(TRP,2)
    })
    # tubuläre Phospatreabsorption mit eGFR-Korrektur
    output$PTubGFR <- renderPrint({
      PSe <- input$P.Serum
      KSe <- input$Krea.Serum
      PUr <- input$P.Urin
      KUr <- input$Krea.Urin
      TRP <- 1-((PUr*KSe)/(PSe*(KUr*1000)))
      if (TRP <= 0.86){
        TmP.Less <- TRP*PSe
        return(round(TmP.Less,2))
      } else {
        TmP.High <- 0.3*(TRP/(1-(0.8*TRP)))*PSe
        return(round(TmP.High),2)
      }
    })
  # QT-Zeit-Korrektur
    output$QTKor <- renderPrint({
      round(input$QT/sqrt(60/input$HR), 0)
    })
  # CHA2DS2-VASc-Score
    output$Chad <- renderPrint({
      HI <- switch(input$HI, HIja = 1, HInein = 0)
      AHT <- switch(input$AHT, AHTja = 1, AHTnein = 0)
      Alter2 <- switch(input$Alter2, Alter2 = 2, Alter1 = 1, Alter0 = 0)
      Diab <- switch(input$Diab, Diabja = 1, Diabnein = 0)
      Stroke <- switch(input$Stroke, Strokeja = 2, Strokenein = 0)
      Frau <- switch(input$Frau, Frauja = 1, Fraunein = 0)
      Vasc <- switch(input$Vasc, Vascja = 1, Vascnein = 0)
      Chad <- HI+AHT+Alter2+Diab+Stroke+Frau+Vasc
      Chad
    })
  # Wells-Score TVT
    output$Wells1 <- renderPrint({
      Mal <- switch(input$Mal, Malja = 1, Malnein = 0)
      Imm <- switch(input$Imm, Immja = 1, Immnein = 0)
      Bett <- switch(input$Bett, Bettja = 1, Bettnein = 0)
      Pain <- switch(input$Pain, Painja = 1, Painnein = 0)
      Schwell <- switch(input$Schwell, Schwellja = 1, Schwellnein = 0)
      Umfang <- switch(input$Umfang, Umfangja = 1, Umfangnein = 0)
      Ödem <- switch(input$Ödem, Ödemja = 1, Ödemnein = 0)
      Koll <- switch(input$Koll, Kollja = 1, Kollnein = 0)
      TVT <- switch(input$TVT, TVTja = 1, TVTnein = 0)
      Alt <- switch(input$Alt, Altja = -2, Altnein = 0)
      Wells1 <- Mal+Imm+Bett+Pain+Schwell+Umfang+Ödem+Koll+TVT+Alt
      Wells1
    })
  # Wells-Score LAE
    output$Wells2 <- renderPrint({
      TVTSymp <- switch(input$TVTSymp, TVTSympja = 3, TVTSympnein = 0)
      LAE <- switch(input$LAE, LAEja = 3, LAEnein = 0)
      HF <- switch(input$HF, HFja = 1.5, HFnein = 0)
      OP <- switch(input$OP, OPja = 1.5, OPnein = 0)
      TVTalt <- switch(input$TVTalt, TVTaltja = 1.5, TVTaltnein = 0)
      Häm <- switch(input$Häm, Hämja = 1, Hämnein = 0)
      Tumor <- switch(input$Tumor, Tumorja = 1, Tumornein = 0)
      Wells2 <- TVTSymp+LAE+HF+OP+TVTalt+Häm+Tumor
      Wells2
    })
  # HAS-BLED-Score
    output$HAS <- renderPrint({
      Hyp <- switch(input$Hyp, Hypja = 1, Hypnein = 0)
      Niere <- switch(input$Niere, Keineja = 0, Niereja = 1, Leberja = 1, Beideja = 2)
      Stroke2 <- switch(input$Stroke2, Stroke2ja = 1, Stroke2nein = 0)
      Blut <- switch(input$Blut, Blutja = 1, Blutnein = 0)
      INR <- switch(input$INR, INRja = 1, INRnein = 0)
      Alter3 <- switch(input$Alter3, Alter3ja = 1, Alter3nein = 0)
      Meds <- switch(input$Meds, Keine2ja = 0, Medsja = 1, Alkja = 1, Beide2ja = 2)
      HAS <- Hyp+Niere+Stroke2+Blut+INR+Alter3+Meds
      HAS
    })
  # Child-Pugh-Punktzahl
    output$Child <- renderPrint({
      Alb <- switch(input$Alb, Alb1 = 1, Alb2 = 2, Alb3 = 3)
      Bili <- switch(input$Bili, Bili1 = 1, Bili2 = 2, Bili3 = 3)
      Quick <- switch(input$Quick, Quick1 = 1, Quick2 = 2, Quick3 = 3)
      Asz <- switch(input$Asz, Asz1 = 1, Asz2 = 2, Asz3 = 3)
      Enc <- switch(input$Enc, Enc1 = 1, Enc2 = 2, Enc3 = 3)
      Child <- Alb+Bili+Quick+Asz+Enc
      Child
    })
  # Grace-Score
    output$Grace <- renderPrint({
      Killip <- switch(input$Killip, Killip1 = 0, Killip2 = 20, Killip3 = 39, Killip4 = 59)
      Sys <- switch(input$Sys, Sys1 = 58, Sys2 = 53, Sys3 = 43, Sys4 = 34, Sys5 = 24, Sys6 = 10, Sys7 = 0)
      HFR <- switch(input$HFR, HFR1 = 0, HFR2 = 3, HFR3 = 9, HFR4 = 15, HFR5 = 24, HFR6 = 38, HFR7 = 46)
      Alter4 <- switch(input$Alter4, Alter41 = 0, Alter42 = 8, Alter43 = 25, Alter44 = 41, Alter45 = 58, Alter46 = 75, Alter47 = 91, Alter48 = 100)
      CreaGrace <- switch(input$CreaGrace, CreaGrace1 = 1, CreaGrace2 = 4, CreaGrace3 = 7, CreaGrace4 = 10, CreaGrace5 = 13, CreaGrace6 = 21, CreaGrace7 = 28)
      Cardiac <- switch(input$Cardiac, Cardiac1 = 39, Cardiac2 = 0)
      ST <- switch(input$ST, ST1 = 28, ST2 = 0)
      Trop <- switch(input$Trop, Trop1 = 14, Trop2 = 0)
      Grace <- Killip+Sys+HFR+Alter4+CreaGrace+Cardiac+ST+Trop
      Grace
    })
}
  
shinyApp(ui = ui, server = server)