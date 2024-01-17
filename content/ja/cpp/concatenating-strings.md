---
title:                "文字列の結合"
html_title:           "C++: 文字列の結合"
simple_title:         "文字列の結合"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

こんにちは！今日は、C++で文字列を連結する方法についてお話ししましょう。プログラマーが文字列を連結する理由ややり方を紹介します。また、深く掘り下げて文字列の連結について詳しくお伝えします。興味のある方は、ぜひ最後の「参考文献」もチェックしてみてくださいね！

## What & Why?

文字列を連結するとは、単純に言えば、二つ以上の文字列を繋げることを意味します。プログラマーは、このような方法を使うことで、より複雑な文字列を生成することができます。例えば、名前と姓を合わせてフルネームを作ったり、文章に変数を埋め込んで動的な内容を作ったりする際に利用されます。

## How to:

文字列を連結する最も簡単な方法は、「+」を使うことです。例を見てみましょう。

```C++
#include <iostream>

using namespace std;

int main() {
  string firstName = "太郎";
  string lastName = "田中";
  string fullName = firstName + lastName;
  
  cout << fullName << endl;
  
  return 0;
}
```

このコードを実行すると、変数fullNameには「太郎田中」という文字列が格納され、コンソールには「太郎田中」という文字列が表示されます。

さらに、文字列を変数ではなく直接繋げることもできます。

```C++
#include <iostream>

using namespace std;

int main() {
  string message = "Hello, " + "World!";
  
  cout << message << endl;
  
  return 0;
}
```

この場合、messageには「Hello, World!」という文字列が格納されます。

## Deep Dive:

文字列を連結する方法には、他にもいくつかの方法があります。例えば、C言語ではstrcatという関数を使うことができます。また、文字列を扱うライブラリを使うことで、より高度な操作が可能になります。

また、文字列を連結する際には、メモリ管理に注意が必要です。コンパイラによっては、多くの文字列を連結するときにメモリが不足する場合があります。そのため、文字列を連結する前に必要なメモリを確保するようにコードを書くことが大切です。

## See Also:

- <a href="https://www.programiz.com/cpp-programming/string">C++の文字列操作</a>
- <a href="https://www.geeksforgeeks.org/string-class-in-cpp/">C++の基本的な文字列操作メソッド</a>