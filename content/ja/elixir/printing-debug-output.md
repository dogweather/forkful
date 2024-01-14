---
title:    "Elixir: デバッグ出力の印刷"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

Japanese translation: 

## なぜ

デバッグ出力を表示することの利点を説明します。 

デバッグ出力は、アプリケーションの動作を理解し、問題を解決するために非常に有用です。コードを実行している間に起こった処理を把握し、プログラムの動きをトラックすることができます。さらに、デバッグ出力には、特定の変数の値や条件を確認することもできます。 

## 使い方 

デバッグ出力を表示するには、Elixirの `IO.inspect` 関数を使用します。以下は、リストの要素を1つずつデバッグ出力する例です。 

```Elixir 
list = [1, 2, 3] 
Enum.each(list, fn x -> 
  IO.inspect(x) 
end) 
```

出力結果: 

```shell 
1 
2 
3 
``` 

## ディープダイブ 

デバッグ出力をより効果的に使用するためには、いくつかのテクニックがあります。 

- デバッグ出力をさまざまな階層で使用することで、プログラムの流れを把握しやすくなります。 
- 特定の変数や条件の値のみをデバッグ出力することもできます。 
- `IO.inspect(, label: "message")` を使用することで、メッセージを表示することができます。 

## 参考リンク

「[Elixirのデバッグ出力](https://elixirschool.com/jp/lessons/basics/io-and-the-stdout/)」 - Elixirスクールの基本レッスン 

「[ElixirのIOモジュール](https://elixir-lang.org/getting-started/io-and-the-file-system.html)」 - 公式ドキュメント 

「[コーディング中の5つのElixirデバッグテクニック](https://divante.com/blog/5-elixir-debugging-tools-to-use-when-the-code-goes-south/)」 - Divanteブログ記事