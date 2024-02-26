---
date: 2024-01-20 17:43:40.913947-07:00
description: "\u4E0B\u8F09\u4E00\u500B\u7DB2\u9801\uFF0C\u5C31\u662F\u628A\u5B83\u5F9E\
  \u7DB2\u4E0A\u642C\u5230\u4F60\u7684\u96FB\u8166\u4E0A\u3002\u7A0B\u5E8F\u54E1\u9019\
  \u9EBC\u505A\u4F86\u5206\u6790\u7DB2\u9801\u5167\u5BB9\u6216\u70BA\u4E86\u5099\u4EFD\
  \u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:45.528942-07:00'
model: gpt-4-1106-preview
summary: "\u4E0B\u8F09\u4E00\u500B\u7DB2\u9801\uFF0C\u5C31\u662F\u628A\u5B83\u5F9E\
  \u7DB2\u4E0A\u642C\u5230\u4F60\u7684\u96FB\u8166\u4E0A\u3002\u7A0B\u5E8F\u54E1\u9019\
  \u9EBC\u505A\u4F86\u5206\u6790\u7DB2\u9801\u5167\u5BB9\u6216\u70BA\u4E86\u5099\u4EFD\
  \u3002"
title: "\u4E0B\u8F7D\u7F51\u9875"
---

{{< edit_this_page >}}

## What & Why? 什麼與為何?
下載一個網頁，就是把它從網上搬到你的電腦上。程序員這麼做來分析網頁內容或為了備份。

## How to: 如何實現
```Bash
# 使用curl命令下載網頁
curl https://example.com -o saved_page.html

# 使用wget命令
wget https://example.com
```
範例輸出：
```Bash
# 如果使用curl
% Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                               Dload  Upload   Total   Spent    Left  Speed
100  1256  100  1256    0     0   6352      0 --:--:-- --:--:-- --:--:--  6400

# 如果使用wget
--2023-04-01 10:00:00--  https://example.com/
解析主機 example.com (example.com)... 93.184.216.34
連接 example.com (example.com)|93.184.216.34|:443... 已連接。
已送出 HTTP 請求，正在等候回應... 200 OK
長度: 1256 (1.2K) [text/html]
正儲存至: 'index.html'

index.html                100%[===================>]   1.23K  --.-KB/s  於 0s       

2023-04-01 10:00:00 (30.4 MB/s) - 'index.html' 已儲存 [1256/1256]
```

## Deep Dive 深入探討
早期，下載網頁通常是手動的。隨著互聯網的發展，自動化工具如 `curl` 和 `wget` 出現，它們可以透過命令列介面操作。`curl` 支持各種協議而 `wget` 偏向於從伺服器上遞迴地下載內容。兩者均允許使用者自定義請求頭部(header)、隨批次檔案執行(batch)等操作。

## See Also 另見
- [curl website](https://curl.se/)
- [wget manual](https://www.gnu.org/software/wget/manual/wget.html)
