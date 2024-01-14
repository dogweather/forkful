---
title:                "Fish Shell: デバッグ出力の印刷"
programming_language: "Fish Shell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

# なぜ

プログラムを書く際に、どのように実行されているかを確認するために、デバッグ出力をプリントすることは非常に役に立ちます。デバッグ出力は、コードがどのように動作しているかを確認し、問題を解決するのに役立つ情報を提供します。

## やり方

デバッグ出力をプリントするには、次のようなコマンドを使います。

```fish
echo "デバッグ出力"
```

上記のコマンドをターミナルで実行すると、"デバッグ出力"という文字列が出力されます。

また、変数の値や条件文を出力することもできます。

```fish
set variable "変数の値"
echo $variable
echo (if expr; "条件が真の場合" else; "条件が偽の場合"; end)
```

次のような出力が得られるでしょう。

```
変数の値
条件が真の場合
または
条件が偽の場合
```

さらに、ファイルの中身やコマンドの結果も出力することができます。

```fish
cat file.txt
ls | grep "キーワード"
```

## 深く掘り下げる

デバッグ出力をプリントすることの利点は、コードを理解しやすくすることにあります。コードが複雑だったり、思った通りに動作しなかったりする場合には、デバッグ出力を追加することで、どのような処理を行っているかを把握しやすくなります。

また、デバッグ出力を使うことで、コードの実行中にどのような変数が使われているかを確認することもできます。これにより、プログラム内のバグを特定するのに役立ちます。

# 見逃さないでください

- [Fish Shell 公式サイト](https://fishshell.com)
- [Fish Shell の使い方](https://github.com/fish-shell/fish-shell/blob/master/doc_src/README.md)
- [Fish Shell のチュートリアル](https://fishshell.com/docs/current/tutorial.html)

---
See Also / 関連リンク:

- [Fish Shell Cookbook](https://github.com/jorgebucaran/fisher)
- [Fish Shell 設定ガイド](https://medium.com/@KiwamuIto/ja007-introducing-the-fish-shell-its-setup-and-configuration-7f6a2bf6f0f)