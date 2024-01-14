---
title:    "C: コンピュータプログラミングにおける「コマンドライン引数の読み込み」"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ

コマンドライン引数の読み取りを行う理由はさまざまですが、主な理由はプログラムの柔軟性を高めるためです。コマンドライン引数を使用することで、ユーザーがプログラムの挙動をカスタマイズしたり、異なる値を入力したりすることができます。

## 実装方法

コマンドライン引数を読み取るためには、まず`main()`関数の引数に`argc`と`argv`を追加します。`argc`はコマンドライン引数の数を表し、`argv`は引数の値を配列として保持します。

以下の例では、コマンドライン引数を計算に使用する数値として取得し、それらの合計を出力するプログラムを示します。

```C
#include <stdio.h> 

int main(int argc, char *argv[]) 
{ 
    int sum = 0; 
  
     // 引数の数だけループを実行
    for(int i = 1; i < argc; i++) 
    { 
        // argv配列から数値を取得してsumに加算
        sum += atoi(argv[i]); 
    } 
  
    // 合計を出力
    printf("合計は %d です。", sum); 
  
    return 0; 
} 
```

コンパイルして実行すると、以下のような結果が得られます。

```
入力: gcc sum.c -o sum
引数: 1 2 3 4 5
出力: 合計は 15 です。
```

## 深堀り

コマンドライン引数は、プログラムを実行する前に変更することができます。例えば、ファイル名やオプションの設定など、プログラムに必要な情報を渡すことができます。また、引数を使用することで、プログラムの実行時にエラーを検出することも可能です。

さらに、コマンドライン引数はデバッグにも役立ちます。デバッグ時に変数の値をハードコーディングする代わりに、コマンドライン引数から値を取得すれば、プログラムを再コンパイルする必要なく変更することができます。

## 関連リンク

- [コード例と詳細な説明 - Tutorialspoint](https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm)
- [コマンドライン引数を使用する際の注意点 - GeeksforGeeks](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [引数の数や内容をバリデーションする方法 - CodeGuru](https://www.codeguru.com/cpp/cpp/cpp_mfc/article.php/c3879/Handling-Command-line-Arguments-as-a-Flexible-and-Powerful-Component-of-any-C-Application.htm)

## ご参考

- [Markdown 記法チートシート](https://qiita.com/Qiita/items/c686397e4a0f4f11683d)