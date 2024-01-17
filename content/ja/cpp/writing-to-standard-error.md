---
title:                "「標準エラーに書き込む」"
html_title:           "C++: 「標準エラーに書き込む」"
simple_title:         "「標準エラーに書き込む」"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

##何で・その目的？

**標準エラー出力 **とは、プログラマーがエラーメッセージを表示するための方法の一つです。これは、コンピューターが動作しないときにデバッグを手助けするために使用されます。プログラマーが問題を解決するために必要な情報を提供することができるため、重要なツールの一つとなっています。

##やり方：

```c++
#include <iostream>
using namespace std;

cerr << "エラーが発生しました" << endl;
```

```c++
エラーが発生しました
```

この例では、```cerr```というコマンドを使用し、エラーメッセージを出力しています。まず、```iostream```をインクルードし、```using namespace std;```を使用して、標準名前空間を指定します。次に、標準エラー出力を行うために```cerr```を使用し、メッセージを出力しています。最後に、```endl```を使用して改行を行います。

##深く掘り下げる：

**標準エラー出力**は、C言語から継承された概念です。以前は、プログラムの実行中にエラーが発生した場合、エラーメッセージは標準出力に表示されていました。しかし、標準出力には通常のプログラム出力も含まれており、エラーメッセージが混ざってしまうことが多々ありました。そのため、標準エラー出力が導入され、エラーメッセージ専用の出力先として使用されるようになりました。

代替方法としては、コンソールに直接エラーメッセージを出力することもできますが、プログラムがファイルなどの出力先を持っている場合、エラーメッセージが見にくくなる可能性があります。

また、標準エラー出力はプログラムの実行中にエラーメッセージを表示できるため、例外処理などの重要な機能を実装する際にも使用されます。

##関連リンク：

- [C++標準ライブラリ](https://cpprefjp.github.io/reference/iostream/ostream/cerr.html)
- [C言語の標準エラー出力](https://www.ibm.com/support/knowledgecenter/ja/ssw_ibm_i_71/rtref/ccerr.htm)
- [C++での例外処理](https://ja.cppreference.com/w/cpp/error/exception)