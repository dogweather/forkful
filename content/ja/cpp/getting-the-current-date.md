---
title:                "現在の日付の取得"
html_title:           "C++: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

＃＃ 今回は、C++の最新版を使って、日本の読者のために、非公式なトーンで、長くなり過ぎずに、要点を伝えるスタイルでプログラミングの記事を書きます。本記事は、４つのセクションに分かれており、それぞれ日本語に翻訳した見出しを持っています。不必要な単語や文は避け、簡潔に書いています。

"## 何か？"
現在の日付を取得するとは、システムにおいて現在の日付を表示することを意味します。プログラマーが現在の日付を取得する理由の一つは、自分のプログラムを実行した時の日付を把握することができるようにするためです。

"## 方法："
```C++
#include <iostream>
#include <ctime>

using namespace std;

int main() {
  // 現在の日付を取得
  time_t now = time(0);
  
  // 現在の日付を文字列に変換
  char* date = ctime(&now);
  
  // 現在の日付の出力
  cout << "現在の日付：" << date << endl;
  
  return 0;
}
```

```
現在の日付：Mon Aug 10 16:47:35 2020
```

"## 詳細を調べる："
現在の日付を取得するには、様々な方法があります。その中でも最も一般的な方法は、C++の```ctime```ライブラリを使う方法です。このライブラリを使うことで、プログラム内で現在の日付を取得し、文字列として表示することができます。また、プログラマーが自分で日付を表示するフォーマットを設定することもできます。他にも、プラットフォームや目的に応じて、さまざまなライブラリやツールを使うこともできます。

## 関連情報
- [C++ reference: time](https://en.cppreference.com/w/cpp/chrono/c/time)