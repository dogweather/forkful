---
title:                "C++: 2つの日付を比較する"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ

日常生活やビジネスで、私たちは日付を比較する必要があります。例えば、誕生日を祝うために誰かの年齢を計算したり、プロジェクトの期限日と今日の日付を比較することで、残りの日数を計算したりすることがあります。このような状況では、日付を比較するプログラムを書くことが非常に便利です。

## 使い方

日付を比較するためには、C++言語で組み込みの機能を使用することができます。以下のコードブロックを参考にしてください。

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main() {
    // 今日の日付を取得
    time_t t = time(NULL);
    tm* now = localtime(&t);
    int today = now->tm_mday;
    int thisMonth = now->tm_mon + 1;    // 月は0から数えるため、1を足す
    
    // 比較する日付を入力
    int inputDay, inputMonth;
    cout << "日付を入力してください (例：12 7)：";
    cin >> inputDay >> inputMonth;
    
    // 日付を比較
    if(inputMonth < thisMonth) {
        cout << "入力した日付は過去の日付です。" << endl;
    } else if(inputMonth == thisMonth && inputDay < today) {
        cout << "入力した日付は過去の日付です。" << endl;
    } else if(inputDay == today && inputMonth == thisMonth) {
        cout << "入力した日付は今日の日付です。" << endl;
    } else {
        cout << "入力した日付は未来の日付です。" << endl;
    }
    
    return 0;
}
```

上記のコードでは、現在の日付を取得し、入力された日付と比較して、どのような関係にあるかを判定しています。実行結果は以下のようになります。

```
日付を入力してください (例：12 7)：15 7
入力した日付は未来の日付です。
```

このように、比較した日付に応じて適切なメッセージを出力することができます。

## ディープダイブ

日付を比較する際には、コンピューターの内部でどのように処理されるかを理解することが重要です。日付というのは数字で表現されていますが、それをコンピューターがどのように解釈するかは文化や歴史的背景によって異なります。そのため、プログラムで使用される日付のフォーマットにも注意する必要があります。

また、日付を比較する際には、既存のライブラリや関数を使用することで、より効率的にプログラムを書くことができます。例えば、C++では`time.h`ライブラリを使用することで、現在の日付を取得したり、日付の計算を行ったりすることができます。

## 参考リンク

- https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm
- https://www.cplusplus.com/reference/ctime/localtime/
- https://ja.wikipedia.org/wiki/日付の表記方法
- https://www.geeksforgeeks.org/comparisons-of-date-and-time-in-c/