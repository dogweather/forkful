---
title:                "Fish Shell: テキストの検索と置換"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# なぜ検索と置換をするのか

テキストの検索と置換は、コマンドラインでファイルを効率的に編集するための重要なスキルです。Fish Shellを使用する際には、これらの機能を上手く活用することで作業の効率を大きく向上させることができます。

# 方法

例えば、特定の文字列を別の文字列に置換したい場合、以下のようなコマンドを使用します。

```
fish -c "sed -i 's/検索文字列/置換文字列/g' ファイル名"
```

このコマンドでは、`sed`コマンドを使用し、`-i`オプションでファイルを直接編集し、`g`オプションでマッチしたすべての文字列を置換します。

また、より複雑な置換を行う場合には、正規表現を使うこともできます。例えば、`[0-9]+`という正規表現を使用することで、数字のみを含む文字列を、別の数字に置換することができます。

```
fish -c "sed -i 's/[0-9]+/置換する数字/g' ファイル名"
```

ここで、`置換する数字`には、置換する数字に置き換えたい任意の数字を指定します。

# 深堀り

検索と置換にはさまざまなパターンやオプションが存在しますので、より詳細な使用方法を学ぶことも可能です。例えば、ファイル内の特定の行のみを対象としたり、大文字と小文字を区別するオプションを使ったりすることができます。

また、Fish Shellには独自の検索と置換コマンドが存在するため、より柔軟性の高い編集が可能です。`builtin`コマンドを使用することで、Fishの検索と置換コマンドを呼び出すことができます。

# See Also

- [Fish Shellドキュメント：検索と置換コマンド](https://fishshell.com/docs/current/commands.html#substitute)
- [sedコマンドのドキュメント](https://www.gnu.org/software/sed/manual/sed.html)
- [正規表現のチートシート](https://www.rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf)