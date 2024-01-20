---
title:                "文字列から日付を解析する"
html_title:           "Bash: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 何と何故？

日付の解析（パーシング）は、文字列から日付情報を抽出するプログラミング技法です。逆に、特定の形式の文字列を日付に変換します。これは、データ入力、ログ分析、データの検証などのために必要です。

## 実践方法：

以下に、Clojureで文字列から日付を解析する基本的な例を示します。

```Clojure
(import '[java.text SimpleDateFormat])
(import '[java.util Date])

(defn string-to-date [s format]
  (.parse (SimpleDateFormat. format) s))
```

例えば、次のコーンドを呼び出してみてください。

```Clojure
(string-to-date "2020/01/02" "yyyy/MM/dd")
```

出力結果:

```Clojure
#inst "2020-01-02T00:00:00.000-00:00"
```

##深掘り情報：

日付解析は古くから存在する技法で、多くの方法とライブラリでサポートされています。Clojureでは、標準的なJavaのDateとSimpleDateFormatクラスを使用する最もシンプルな方法ですが、joda-timeライブラリの使用も一般的です。

詳細としては、SimpleDateFormatの文字列フォーマットがあります。これは、どの形式の文字列がどのような日付データへと変換されるべきかを決定します。たとえば、「yyyy/MM/dd」は年/月/日の形式になります。

##参考情報：

以下に、日付の解析に関連するいくつかのリンクを掲載します。

1. Clojure日付操作のガイド：https://clojure-cookbook.com/ 

2. 日付形式について：https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html

3. Clojureでの日付と時間の操作（英語）：https://www.baeldung.com/clojure-date-time