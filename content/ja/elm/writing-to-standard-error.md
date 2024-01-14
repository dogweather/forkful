---
title:    "Elm: 「標準エラーへの書き込み」"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ

エラーメッセージは、コンピュータープログラムを作成する際に不可欠です。プログラムの実行中にエラーが発生した場合、その詳細を特定して修正するためにエラーメッセージを使用します。 Elmでは、標準エラー出力を使用して、プログラム内で発生したエラーを特定し、解決することができます。 

## 方法

標準エラー出力に書き込むには、 ```Elm.io.stderr``` モジュールを使用します。以下のコードブロックに例を示します。 

```Elm
import IO

main =
  IO.stderr "エラーが発生しました。"
```

上記の例では、```IO.stderr```関数を使用して、エラーメッセージを標準エラー出力に書き込んでいます。これにより、実行中にエラーが発生した場合、それを特定することができます。 

## 深堀り

標準エラー出力を使用することで、プログラム内で発生したエラーを特定できるだけでなく、エラーメッセージをカスタマイズすることもできます。例えば、エラーメッセージにファイル名や行数を追加することで、特定の箇所で発生したエラーをすばやく特定することができます。また、複数のエラーメッセージを書き込むこともでき、それらを区別することができます。詳細は、公式ドキュメントを参照してください。 

## それでは参考に

- [Elm 公式ドキュメント](https://guide.elm-lang.jp/error_handling/standard_error_output.html)
- [「標準エラー出力を使用してエラーメッセージをカスタマイズする」](https://teratail.com/questions/234646)
- [「標準エラー出力を使用する方法」](https://medium.com/@nenu7/using-standard-error-output-in-elm-a5c95189cc60)

# 参考リンク