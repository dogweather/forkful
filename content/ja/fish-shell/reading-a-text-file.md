---
title:    "Fish Shell: テキストファイルを読み込む"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

### なぜ読むのか

テキストファイルを読むことで、コンピューターが扱いやすい形式に情報を整理することができます。

### 読み込み方

テキストファイルを読み込むには、Fish Shellの`read`コマンドを使用します。以下のコード例を参考にしてください。

```
read -f input.txt  # "input.txt"という名前のファイルを読み込む

while read line  # ファイルの各行を読み込むループを作成
echo $line  # 各行を表示
end
```

上記のコード例を実行すると、ファイルから読み取った情報を1行ずつ表示することができます。

### 深堀り

テキストファイルを読み込む際には、ファイルの形式やエンコーディングに注意する必要があります。また、ファイルが大きすぎる場合は、エラーが発生する可能性があるため、適切な方法でファイルを読み込むことが重要です。

### 参考リンク

[Fish Shell Documentation - read command](https://fishshell.com/docs/current/cmds/read.html)  
[How to Read a Text File in Fish Shell](https://www.tecmint.com/read-text-file-in-fish-shell/)