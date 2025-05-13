# Shiny-Trade-Market-Project
Shiny Trade Market Project` est une application **R Shiny** interactive permettant d’analyser l’impact d’un événement économique (ex : guerre commerciale) sur les marchés financiers.

Cette version intègre :
- **NASDAQ**
- **Euro Stoxx 50**
- **Forex EUR/USD**
- **Nikkei 225**
- **Or (Gold)**
- **Obligations US 10Y**

Vous pouvez visualiser :
- Cours avec Moyenne Mobile
- Histogrammes des rendements
- Densité des rendements
- Boxplots
- Rendements cumulés
- Volume des transactions
- Tendance LOESS
- Tableaux récapitulatifs de volatilité et rendements

---

## 📥 Installation

### 1️⃣ Prérequis

- R (>= 4.0)
- RStudio (recommandé)
- Packages : `shiny`, `quantmod`, `ggplot2`, `zoo`, `dplyr`, `scales`, `tidyr`, `DT`

### 2️⃣ Installer les packages

```R
install.packages(c("shiny", "quantmod", "ggplot2", "zoo", "dplyr", "scales", "tidyr", "DT"))
