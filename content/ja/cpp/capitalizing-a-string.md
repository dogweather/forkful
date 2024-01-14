---
title:                "C++: 文字列を大文字にする"
simple_title:         "文字列を大文字にする"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

今回は、C++で文字列の大文字化を行う方法についてご紹介します。文字列の大文字化は、プログラミングにおいて非常によく使用される処理の一つです。例えば、ユーザーから入力された文字列を大文字化して処理したい場合などに活用することができます。

## 方法

文字列の大文字化を行うには、C++の標準ライブラリであるstringライブラリを使用します。具体的なコーディング例を以下に示します。

```C++
// 文字列を大文字化する関数
string capitalize(string str) {
  // 文字列の長さを取得
  int length = str.length();
  // 文字列の各文字をループし、大文字に変換する
  for (int i = 0; i < length; i++) {
    str[i] = toupper(str[i]);
  }
  // 変換した文字列を返す
  return str;
}

int main() {
  // ユーザーから文字列を入力してもらう
  string inputStr;
  cout << "文字列を入力してください: ";
  getline(cin, inputStr);
  // capitalize関数を呼び出し、入力された文字列を大文字化する
  string outputStr = capitalize(inputStr);
  // 変換した文字列を出力する
  cout << "変換後の文字列: " << outputStr;
  return 0;
}
```

上記のコードを実行すると、ユーザーから入力した文字列が大文字化されて出力されることが分かります。例えば、入力した文字列が"hello world"の場合、出力される結果は"HELLO WORLD"になります。

## 詳細を調べる

文字列を大文字化するために使用したtoupper関数は、文字列を受け取って大文字に変換して返す関数です。また、stringライブラリには他にも様々な便利な関数が用意されていますので、ぜひ調べてみてください。

## それではまた次回！

今回は、C++で文字列の大文字化を行う方法についてご紹介しました。文字列の大文字化は、プログラミングにおいて非常に便利な処理の一つですので、ぜひ活用してみてください！

## 関連リンク

- [C++ Reference – toupper関数](https://en.cppreference.com/w/cpp/string/byte/toupper)
- [C++ Reference - stringライブラリ](https://en.cppreference.com/w/cpp/string/basic_string)
- [C++入門サイト「Programming Place Plus」](https://programming-place.net/ppp/contents/cpp/index.html)