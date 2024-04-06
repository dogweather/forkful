---
date: 2024-01-20 17:43:40.913947-07:00
description: "How to: \u5982\u4F55\u5BE6\u73FE \u65E9\u671F\uFF0C\u4E0B\u8F09\u7DB2\
  \u9801\u901A\u5E38\u662F\u624B\u52D5\u7684\u3002\u96A8\u8457\u4E92\u806F\u7DB2\u7684\
  \u767C\u5C55\uFF0C\u81EA\u52D5\u5316\u5DE5\u5177\u5982 `curl` \u548C `wget` \u51FA\
  \u73FE\uFF0C\u5B83\u5011\u53EF\u4EE5\u900F\u904E\u547D\u4EE4\u5217\u4ECB\u9762\u64CD\
  \u4F5C\u3002`curl` \u652F\u6301\u5404\u7A2E\u5354\u8B70\u800C `wget`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.262295-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u5BE6\u73FE \u65E9\u671F\uFF0C\u4E0B\u8F09\u7DB2\u9801\u901A\
  \u5E38\u662F\u624B\u52D5\u7684\u3002\u96A8\u8457\u4E92\u806F\u7DB2\u7684\u767C\u5C55\
  \uFF0C\u81EA\u52D5\u5316\u5DE5\u5177\u5982 `curl` \u548C `wget` \u51FA\u73FE\uFF0C\
  \u5B83\u5011\u53EF\u4EE5\u900F\u904E\u547D\u4EE4\u5217\u4ECB\u9762\u64CD\u4F5C\u3002\
  `curl` \u652F\u6301\u5404\u7A2E\u5354\u8B70\u800C `wget` \u504F\u5411\u65BC\u5F9E\
  \u4F3A\u670D\u5668\u4E0A\u905E\u8FF4\u5730\u4E0B\u8F09\u5167\u5BB9\u3002\u5169\u8005\
  \u5747\u5141\u8A31\u4F7F\u7528\u8005\u81EA\u5B9A\u7FA9\u8ACB\u6C42\u982D\u90E8(header)\u3001\
  \u96A8\u6279\u6B21\u6A94\u6848\u57F7\u884C(batch)\u7B49\u64CD\u4F5C\u3002"
title: "\u4E0B\u8F7D\u7F51\u9875"
weight: 42
---

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
