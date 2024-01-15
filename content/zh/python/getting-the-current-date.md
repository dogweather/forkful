---
title:                "获取当前日期"
html_title:           "Python: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 為什麼

現在的日期是程式設計中非常常見的一個要素。它可以用於許多不同的應用，例如日期計算、日程安排和檔案命名等。繼續閱讀本文，您將學習如何在 Python 中獲取當前日期。

## 如何

首先，您需要匯入 Python 中的 datetime 模組。這個模組提供了一個 datetime 物件，可用於處理日期和時間。

```Python
import datetime
```

現在，讓我們使用 datetime 模組中的 today() 方法來獲取當前日期。

```Python
today = datetime.date.today()
```

透過呼叫 today() 方法，我們將現在的日期賦值給名為 today 的變數。您可以使用 print() 方法來印出今天的日期。

```Python
print(today)
```

輸出將類似於以下結果。

```
2021-09-28
```

您也可以使用 strftime() 方法來自訂日期的格式。例如，如果您想要以「年/月/日」的格式印出日期，您可以使用以下的程式碼。

```Python
formatted_today = today.strftime('%Y/%m/%d')
print(formatted_today)
```

輸出將類似於以下結果。

```
2021/09/28
```

## 深入探討

除了今天的日期，您也可以使用 datetime 模組來獲取當前的時間。您可以使用 now() 方法來獲取當前的日期和時間。

```Python
now = datetime.datetime.now()
```

透過呼叫 now() 方法，我們將現在的日期和時間賦值給名為 now 的變數。您可以使用 print() 方法來印出現在的日期和時間。

```Python
print(now)
```

輸出將類似於以下結果。

```
2021-09-28 09:30:00.042315
```

除了 today() 和 now() 方法，datetime 模組還提供了許多其他方法來處理日期和時間。您可以閱讀官方文件以深入了解。

## 參考連結

- 官方文檔：https://docs.python.org/3/library/datetime.html
- 維基百科：https://zh.wikipedia.org/zh-hant/Python
- 廖雪峰的 Python3 教程：https://www.liaoxuefeng.com/wiki/1016959663602400
- 動手學 Python：https://www.bilibili.com/video/BV1ux411Z7Tc