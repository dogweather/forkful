---
title:    "Gleam: 日付を文字列に変換する"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## なぜ
日付を文字列に変換することの理由は単純です。日付をより分かりやすく表示したり、様々なシステムでのデータの整理や比較のために使用されるためです。

## 方法
まずは日付を文字列に変換するために、`format`モジュールをインポートします。その後、変換したい日付を指定して、`format.date`関数を使用します。

```Gleam
import format

let date = Date() // 現在の日付を取得
let string = format.date(date, "YYYY-MM-DD") // 文字列に変換
```

上記の例では、現在の日付を指定されたフォーマットに従って文字列に変換しています。`YYYY`は年を表し、`MM`は月を、`DD`は日を表します。このように、フォーマットを自由に指定することができます。

## ディープダイブ
日付を文字列に変換する際には、フォーマットについて細かく知る必要があります。フォーマットには様々なオプションがあり、日付と時間を詳細に指定することができます。

例えば、`YYYY`は4桁の年を表す一方、`YY`は2桁の年を表します。また、`DD`は二桁の日を表す一方、`D`は一桁の日を表します。豊富なオプションを中身することで、フォーマットの自由度が高くなります。

## See Also
- [GleamのDateモジュール](https://gleam.run/modules/date)
- [フォーマットオプションの一覧](https://www.gnu.org/software/gettext/manual/html_node/Locale-Data.html#Locale-Data)
- [Gleamのformatモジュールのソースコード](https://github.com/gleam-lang/gleam_stdlib/blob/master/lib/format/src/format.gleam)