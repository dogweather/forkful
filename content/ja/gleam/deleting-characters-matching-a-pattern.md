---
title:    "Gleam: パターンにマッチする文字を削除する"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## なぜ

この記事では、特定のパターンに一致する文字を削除する際の方法を紹介します。例えば、特定の言語の文書からコードを自動的に削除する場合などに役立ちます。

## 削除方法

Gleamでは、 `String.replace` 関数を使用して文字を削除することができます。以下のコードを参考にしてください。

```Gleam
str = "この文章からコードを削除します"
pattern = "コード"
replaced = String.replace(pattern, str, "")
IO.print_line(replaced)
```

出力結果:

```
この文章からを削除します
```

## 深い掘り下げ

文字を削除する際には、正規表現を使用することもできます。正規表現では、特定のパターンに一致する文字列を検索し、置換することができます。 `Regex.replace` 関数を使用することで、より詳細な文字の削除が可能になります。

また、Gleamではパターンマッチングを使用して、より柔軟に文字を削除することもできます。詳細については、公式ドキュメントを参照してください。

## See Also

- [Gleam公式ドキュメント](https://gleam.run/documentation/)
- [正規表現の基礎](https://www.w3schools.com/python/python_regex.asp)
- [パターンマッチングの使用例](https://gleam.run/examples/pattern_matching/)