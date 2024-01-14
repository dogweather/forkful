---
title:    "Bash: 日付を文字列に変換する"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

##なぜ

Bashプログラミングの中で日付を文字列に変換する必要性は、コンピューターは日付情報を数字の形式で扱うため、人間が理解しやすい形式に変換するためです。

##方法

まずは、今日の日付を文字列に変換してみましょう。```Bash
date +"今日は %Y年%m月%d日です。"
``` 
出力結果は以下のようになります。
```
今日は 2021年08月31日です。
```
次に、特定の日付を指定して、その日の日付を文字列に変換してみましょう。
```Bash
date -d "2021-10-25" +"%Aは、%B %d, %Yです。"
```
出力結果は以下のようになります。
```
月曜日は、10月 25, 2021です。
```

##深く掘り下げる

日付を文字列に変換する際には、特定の変換指定子を使用する必要があります。例として、```%Y```は年を4桁、```%y```は年を下2桁、```%m```は月を2桁、```%d```は日を2桁で表すことができます。また、より詳細な日付や時刻を表示したい場合には、変換指定子の前に```-```を付けることで指定することができます。

さらに詳しく知りたい方は、Bashのマニュアルページを参照することで、さまざまな変換指定子やオプションを学ぶことができます。

##関連リンク

[Bashのマニュアルページ](https://www.gnu.org/software/bash/manual/bash.html)

[Dateコマンドのマニュアルページ](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)

[日付の変換指定子一覧](https://wiki.bash-hackers.org/commands/builtin/date)