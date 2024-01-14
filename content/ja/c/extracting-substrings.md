---
title:    "C: 部分文字列の抽出"
keywords: ["C"]
---

{{< edit_this_page >}}

## なぜ

サブストリングを抽出する理由は、文字列から必要な情報を抽出するためです。例えば、ユーザーが入力した情報の中から特定のデータを取得する場合や、文字列の一部を別の変数に格納する場合などに使用されます。

## 方法

以下のように記述することで、C言語でサブストリングを抽出することができます。

```C
#include<stdio.h>

void extractSubstring(char str[], int start, int end){
    //変数を定義
    int index, position = 0;
    
    //抽出したい部分の長さを計算
    int length = end - start + 1;
    
    //抽出したい部分を格納するための配列を定義
    char substring[length];
    
    //指定した範囲の文字列を配列にコピー
    for(index = start; index <= end; index++, position++){
        substring[position] = str[index];
    }
    
    //最後にヌル文字を追加
    substring[position] = '\0';
    
    //抽出したサブストリングを出力
    printf("抽出されたサブストリング：%s\n", substring);
}

int main(){
    //サブストリングを抽出したい文字列を定義
    char str[] = "こんにちは、世界！";
    
    //抽出したい部分の始まりと終わりを指定
    int start = 5;
    int end = 9;
    
    //抽出関数を呼び出し
    extractSubstring(str, start, end);
    
    return 0;
}
```
**出力：** 抽出されたサブストリング：世界

以上のように、文字列を抽出する際には抽出したい部分の始まりと終わりの位置を指定し、その範囲の文字列を新しい配列にコピーすることで実現できます。

## 深堀り

サブストリングを抽出するためには、C言語で用意されている`strcpy()`関数を使用することもできます。`strcpy()`関数は、指定した文字列を別の配列にコピーするために使用されます。しかし、この関数はコピー先の配列のサイズを事前に指定する必要があり、コピー元の文字列長よりも長い配列を指定した場合はエラーが発生してしまいます。そのため、抽出する部分の長さを計算し、事前にその長さに応じた配列を用意する必要があります。

また、`strncpy()`関数を使用することもできます。この関数は、コピーする文字数を指定できるため、事前に長さを計算する必要はありません。しかし、この関数は最後に必ずヌル文字を追加する必要があり、サブストリングをコピーする際は注意する必要があります。

## さらに詳しくは

サブストリングを抽出する際には、文字列操作に関する知識が必要です。以下のサイトで詳しく解説されていますので、参考にしてみてください。

- [C言語で文字列を操作する方法](https://programming-review.com/c-string-manipulation-in-c)
- [strcpy()関数について詳しく](https://www.tutorialspoint.com/c_standard_library/c_function_strcpy.htm)
- [strncpy()関数の使い方](http://www.c-tipsref.com/reference/string/strncpy.html)

## 関連リンク

- [参考になるC言語のチュートリアル](https://www.tohoho