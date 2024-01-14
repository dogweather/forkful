---
title:    "Gleam: シリングを小文字に変換する"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

私たちの日常生活では、様々な言語を使用しています。しかし、コンピューターではすべての文字を同じように扱うことができません。そのため、文字列を小文字に変換することはとても重要です。そこで今回は、Gleamで文字列を小文字に変換する方法についてご紹介します。

## Why

文字列を小文字に変換することは、データの整形や比較など、様々なプログラミングの機能において重要です。また、複数の文字列を比較する場合には、すべての文字列を同じ形式に変換する必要があります。そのため、Gleamでは文字列を小文字に変換する機能が重要な役割を果たしています。

## How To

Gleamでは、文字列を小文字に変換するためのencodingモジュールが用意されています。以下のコード例を参考にしてください。

```Gleam
import gleam/encoding

let string = "Gleam Blog Post"

let lower_case_string = encoding.ascii_to_lower(string)

assert lower_case_string == "gleam blog post"
```

上記のコードでは、文字列を小文字に変換するために`ascii_to_lower`関数を使用しています。この関数は、与えられた文字列をASCII文字コードに変換し、それらを小文字に変換して新しい文字列を返します。

## Deep Dive

Gleamで文字列を小文字に変換する際には、encodingモジュール内の`ascii_to_lower`関数が使用されます。この関数は内部的に`table`と呼ばれる変数を使用しており、ASCII文字コードと小文字の対応を定義しています。そのため、この関数では文字列がASCII文字に限定されてしまいますが、日常的に使用する文字列であれば問題はありません。

## See Also

- [Gleam encodingモジュール](https://gleam.run/modules/gleam/encoding.html)
- [ASCII文字コード表](https://ascii.cl/)
- [Gleam公式ドキュメント](https://gleam.run/documentation/)

以上でGleamにおける文字列の小文字変換についてご紹介しました。ぜひこの記事を参考にして、あなたのプログラミングスキルを向上させてください。ありがとうございました！