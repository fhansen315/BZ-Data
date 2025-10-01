# BZ-Data 🚀

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Quarto Publish](https://github.com/fhansen315/BZ-Data/actions/workflows/quarto-publish.yml/badge.svg)](https://github.com/fhansen315/BZ-Data/actions/workflows/quarto-publish.yml)
[![R Check](https://github.com/fhansen315/BZ-Data/actions/workflows/r-check.yml/badge.svg)](https://github.com/fhansen315/BZ-Data/actions/workflows/r-check.yml)
[![GitHub issues](https://img.shields.io/github/issues/fhansen315/BZ-Data)](https://github.com/fhansen315/BZ-Data/issues)
[![GitHub stars](https://img.shields.io/github/stars/fhansen315/BZ-Data)](https://github.com/fhansen315/BZ-Data/stargazers)

Comprehensive data analysis and visualization workspace for FX modality analytics. Built with Quarto, RMarkdown, and Shiny for deep-dive financial institution analysis.

## 🎯 Features

- **FI-Specific Analysis**: Interactive institution selector with time series analysis across all FX modalities (Spot, Forward, Swap, Options)
- **Detailed EDA**: Separate exploratory data analysis page for each FX modality with distributions, trends, and statistical summaries
- **Quarto Site**: Beautiful, interactive reports with plotly visualizations
- **RMarkdown Notebooks**: Parameterized analysis templates
- **Shiny Dashboard**: Real-time interactive analytics with date range filtering and downloads
- **Utility Functions**: Reusable R functions for data loading, plotting, and analysis

## 📁 Project Structure

```
BZ-Data/
├── data/
│   ├── raw/              # Raw data files (CSV, Excel)
│   └── processed/        # Cleaned/transformed data
├── notebooks/
│   ├── quarto/           # Quarto documents (.qmd)
│   └── rmarkdown/        # RMarkdown notebooks (.Rmd)
├── apps/
│   └── shiny/            # Shiny applications
├── R/                    # Reusable R functions
│   ├── data_loader.R     # Data loading utilities
│   ├── plotting.R        # Plotting functions
│   └── utils.R           # General utilities
├── reports/
│   └── figures/          # Generated plots and figures
├── tests/                # Unit tests
├── _quarto.yml           # Quarto configuration
├── Makefile              # Common tasks automation
└── README.md
```

## 🚀 Quick Start

### Prerequisites

- R (>= 4.0)
- Quarto CLI
- Git LFS (for large data files)

### Installation

```bash
# Clone the repository
git clone https://github.com/fhansen315/BZ-Data.git
cd BZ-Data

# Install Git LFS (if not already installed)
git lfs install

# Install R dependencies
make install
# OR manually:
Rscript -e "install.packages('renv'); renv::restore()"
```

### Usage

#### Preview Quarto Site
```bash
make preview
# OR
quarto preview
```

#### Render Quarto Site
```bash
make render
# OR
quarto render
```

#### Run Shiny App
```bash
make shiny
# OR
Rscript -e "shiny::runApp('apps/shiny')"
```

#### Clean Generated Files
```bash
make clean
```

## 📊 Data Format

The project expects data with the following columns:

| Column      | Type   | Description                          |
|-------------|--------|--------------------------------------|
| date        | Date   | Transaction date                     |
| institution | String | Financial institution name           |
| modality    | String | FX modality (Spot/Forward/Swap/Options) |
| value       | Numeric| Transaction value                    |

### Loading Your Data

1. **CSV**: Place your file in `data/raw/` and set environment variable:
   ```bash
   export BZ_DATA_PATH="data/raw/your_file.csv"
   ```

2. **Excel**: Place your `.xlsx` file in `data/raw/` and set:
   ```bash
   export BZ_DATA_PATH="data/raw/your_file.xlsx"
   ```

3. **Default**: The project includes sample data at `data/raw/sample_fx_data.csv`

## 🛠️ Development

### Running Tests
```bash
make test
```

### Linting
```bash
make lint
```

### Adding New Analysis

1. Create a new `.qmd` file in `notebooks/quarto/`
2. Add navigation entry in `_quarto.yml`
3. Use utility functions from `R/` directory
4. Commit and push changes

## 📦 Key Dependencies

- **Data**: `readr`, `readxl`, `dplyr`, `tidyr`
- **Visualization**: `ggplot2`, `plotly`, `patchwork`
- **Reporting**: `quarto`, `rmarkdown`, `knitr`
- **Interactive**: `shiny`, `bslib`, `DT`
- **Utilities**: `scales`, `lubridate`

## 🤝 Contributing

We welcome contributions! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for detailed guidelines.

Quick start:
1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'feat: add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

See [CHANGELOG.md](CHANGELOG.md) for version history.

## 📝 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 👤 Author

**Chicão** ([@fhansen315](https://github.com/fhansen315))

## 🙏 Acknowledgments

Built with ❤️ using Quarto, R, and Shiny.

---

**Note**: This is an active development project. Features and structure may evolve rapidly. Always pull latest changes before starting work.

