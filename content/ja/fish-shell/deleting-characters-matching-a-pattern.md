---
title:                "パターンにマッチする文字を削除する"
html_title:           "Fish Shell: パターンにマッチする文字を削除する"
simple_title:         "パターンにマッチする文字を削除する"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何でやるの？ 
パターンにマッチする文字を削除する、とはどういうことか？プログラマーがこれを行うのはなぜでしょうか？ 

パターンにマッチする文字を削除するとは、ある文字列から特定の文字パターンを探し出し、それらを削除して新しい文字列を作ることです。プログラマーがこれを行うのは、文書やコードを整形するためや、情報の抜粋を行うためなど、さまざまな目的があります。

## どうやるの？ 
```Fish Shell```のコードブロック内に、コーディング例と出力のサンプルを示します。 

例1: ```string match -r 'abc' 'this is abcdefg'``` 

出力1: ```this is defg``` 

例2: ```string match -d -r '[0-9]+' 'the price is $100'``` 

出力2: ```the price is $``` 

## 詳しく調べる 
### 歴史的背景 
文字列の操作は、コンピューターの発展とともに発展してきた分野です。古くはAsciiコードに基づいた処理が行われていましたが、現代ではUnicodeなど文字コードの規格が統一されており、より多様な文字を扱うことが可能になりました。 

### 代替手段 
パターンにマッチする文字を削除する方法は、様々なプログラミング言語やツールで実装されています。たとえば、Perlでは正規表現を使って文字列操作を行うことができます。また、Unixコマンドの```sed```や```awk```などでも同様の操作を行うことができます。 

### 実装の詳細 
```Fish Shell```では、パターンにマッチする文字を削除するために、```string match -r```コマンドを使用します。このコマンドは、文字列内のパターンにマッチする部分を置換することで削除を行います。パターンには正規表現などを使用することができます。詳細な使用方法やオプションについては、公式ドキュメントを参照してください。 

## 関連リンク 
公式ドキュメント: https://fishshell.com/docs/current/cmds/string.html#match 

正規表現について: https://www.gnu.org/software/sed/manual/html_node/Regular-Expressions.html 

Perlについて: https://www.perl.org/