# 青岛水产品销售额与零售额时间序列建模分析

本项目使用 R 语言对青岛 2019-2023 年水产品销售额与零售额月度累计数据进行时间序列建模分析。项目完整实现了从数据清洗、探索性分析、平稳性检验、SARIMA/ETS 建模、模型诊断到未来预测的全流程。

---

## 项目结构

```text
.
├── data/
│ ├── 青岛水产品销售额零售额.xlsx    # 原始数据（因涉及政策原因需要公开申请获取，在此无法提供，需自行放置）
│ ├── processed_aquatic_timeseries.csv  # 清洗后的数据（自动生成）
│ ├── stationarity_tests_r.csv        # 平稳性检验结果（自动生成）
│ └── forecast_2024_r.csv             # 2024年预测结果（自动生成）
├── code/
│ ├── 01_data_preprocess_exploration.R # 数据清洗、描述统计与探索性图形
│ ├── 02_stationarity_and_spectrum.R   # 平稳性检验、ACF/PACF、季节分解与频谱分析
│ ├── 03_model_selection_forecast.R    # SARIMA/ETS 建模、参数估计与预测
│ └── 04_model_diagnostics.R           # 残差诊断、Ljung-Box 检验与误差分析
├── figures/                          # 分析图表存放目录（自动生成）
└── README.md



