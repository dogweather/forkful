---
aliases:
- /zh/python/starting-a-new-project/
date: 2024-01-20 18:04:24.324549-07:00
description: "\u958B\u59CB\u4E00\u500B\u65B0\u9805\u76EE\u5C31\u662F\u5275\u5EFA\u4E00\
  \u500B\u5168\u65B0\u7684\u7A0B\u5F0F\u78BC\u57FA\u790E\u3002\u7A0B\u5E8F\u54E1\u9019\
  \u9EBC\u505A\u662F\u70BA\u4E86\u89E3\u6C7A\u7279\u5B9A\u554F\u984C\uFF0C\u6216\u8005\
  \u958B\u767C\u4E00\u500B\u6709\u8DA3\u7684\u60F3\u6CD5\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:58.791513
model: gpt-4-1106-preview
summary: "\u958B\u59CB\u4E00\u500B\u65B0\u9805\u76EE\u5C31\u662F\u5275\u5EFA\u4E00\
  \u500B\u5168\u65B0\u7684\u7A0B\u5F0F\u78BC\u57FA\u790E\u3002\u7A0B\u5E8F\u54E1\u9019\
  \u9EBC\u505A\u662F\u70BA\u4E86\u89E3\u6C7A\u7279\u5B9A\u554F\u984C\uFF0C\u6216\u8005\
  \u958B\u767C\u4E00\u500B\u6709\u8DA3\u7684\u60F3\u6CD5\u3002"
title: "\u5F00\u59CB\u4E00\u4E2A\u65B0\u9879\u76EE"
---

{{< edit_this_page >}}

## What & Why? (是什麼？為什麼？)
開始一個新項目就是創建一個全新的程式碼基礎。程序員這麼做是為了解決特定問題，或者開發一個有趣的想法。

## How to: (怎麼做：)
```Python
# 建立一個新項目

# 1. 建立一個新的Python虛擬環境
python -m venv my_project_env

# 2. 啟動虛擬環境
# 在Windows上:
my_project_env\Scripts\activate
# 在Unix或MacOS上:
source my_project_env/bin/activate

# 3. 安裝需要的套件
pip install flask

# 4. 創建一個app.py檔案，並寫入以下代碼
# app.py
from flask import Flask
app = Flask(__name__)

@app.route('/')
def hello_world():
    return 'Hello, World!'

# 5. 運行應用程序
# 設定環境變量
export FLASK_APP=app
# 啟動伺服器
flask run

# 瀏覽器顯示結果
* Running on http://127.0.0.1:5000/
```

## Deep Dive (深入探討)
開始新項目不僅限於編寫代碼。它的歷史背後包含了從版本控制的實踐（如git初始化）到專案管理工具的選擇（比如Jira或Trello）。選擇正確的框架和庫（例如Flask或Django）可以決定項目的起步速度。實現細節也關鍵，比如配置開發環境、解決依賴性問題和持續集成設置。

## See Also (延伸閱讀)
- [Flask 官方文檔](https://flask.palletsprojects.com/en/latest/)
- [Python 虛擬環境指南](https://docs.python.org/3/library/venv.html)
- [Python 官方教程](https://docs.python.org/3/tutorial/index.html)
- [Git 版本控制入門](https://git-scm.com/book/en/v2/Getting-Started-Git-Basics)
