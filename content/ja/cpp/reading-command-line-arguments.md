---
title:    "C++: コマンドライン引数の読み取り"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ

みなさんはプログラミングをする際、コマンドライン引数を読み込むことがありますか？コマンドライン引数を読み込むことによって、プログラムをより柔軟に動かすことができます。

## 方法

コマンドライン引数を読み込むには、main関数の引数に**int argc**と**char* argv[]**を指定します。**argc**は引数の数を示し、**argv[]**には引数が格納されます。例えば、以下のように書くことができます。

```C++
int main(int argc, char* argv[]) {
    // 引数があるかどうかを確認する
    if (argc > 1) {
        // argv[0]は実行ファイルの名前なので、それ以降の引数を取得する
        for (int i = 1; i < argc; i++) {
            // argv[i]に格納されている引数を出力する
            cout << argv[i] << endl;
        }
    }
    return 0;
}
```

上記の例では、実行ファイルの名前を除いた引数が全て出力されます。例えば、コマンドラインで"program.exe hello world"を実行した場合、"hello"と"world"が出力されます。

## 深堀り

既存のコマンドライン引数を受け取るだけでなく、自分で新しい引数を定義することも可能です。その際には、stdio.hヘッダーファイルに定義されている**getopt**関数を使用します。この関数を使用すると、より高度なコマンドライン引数の処理が可能になります。

## 参考リンク

- [C++のコマンドライン引数の読み方](https://qiita.com/ta9ya/items/1e769aad844ad1b7b35a)
- [C++のコマンドライン引数を解析する方法](http://kmaebashi.com/wordpress/2012/08/27/how-to-analyze-the-command-line-arguments-of-c/)
- [C++のコマンドライン引数処理の基礎](https://www.tmp1024.com/programming/commandline/)
- [C++でコマンドライン引数を自在に解析する手段](https://www.leko.jp/archives/417)