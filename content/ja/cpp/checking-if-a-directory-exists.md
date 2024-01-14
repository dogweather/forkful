---
title:    "C++: ディレクトリが存在するかどうかをチェックする"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# なぜ
ディレクトリが存在するかどうかを確認することの重要性は、プログラミングにおいて非常に大切です。ディレクトリが存在するかどうかをチェックすることで、プログラムの実行中に意図しないエラーが起きることを防ぐことができます。

## ハウツー
ディレクトリが存在するかどうかをチェックするためのC++のコーディング例を以下に示します。また、サンプルの出力も掲載します。

```C++
#include <iostream>
#include <filesystem>
using namespace std;

int main() {
    string directoryPath = "c:/Documents";
    
    // ディレクトリが存在するかどうかをチェックする
    if (filesystem::exists(directoryPath)) {
        cout << "ディレクトリが存在します。" << endl;
    } else {
        cout << "ディレクトリが存在しません。" << endl;
    }
    
    return 0;
}

```

出力：
```
ディレクトリが存在します。
```

## ディープダイブ
ディレクトリの存在をチェックする方法には様々な手法がありますが、一般的なものは`filesystem::exists()`関数を使用する方法です。この関数は、引数としてチェックしたいディレクトリのパスを受け取り、ディレクトリが存在するかどうかを真偽値で返します。また、`filesystem`ライブラリはC++17から標準ライブラリとして追加されました。

## その他の関連リンク
- [C++標準ライブラリ：ファイルシステム操作](https://cpprefjp.github.io/reference/filesystem.html)
- [ディレクトリが存在するかどうかをチェックする方法（Qiita）](https://qiita.com/uzuki05/items/9c6d100ba3fab0cee590)
- [C++のfilesystemライブラリ - ディレクトリの操作（TechTarget）](https://searchwindevelopment.techtarget.com/ja/tip/FileSystem-Library-in-C-filesystem-library)