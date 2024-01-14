---
title:    "C++: ディレクトリが存在するかどうかをチェックする"
keywords: ["C++"]
---

{{< edit_this_page >}}

## なぜ"ディレクトリの存在を確認する"必要があるのか
ディレクトリの存在を確認することは、プログラムが実行される環境において、ファイルの読み書きや検索を行う際に重要な作業です。もしディレクトリが存在しない場合、プログラムが意図しない結果を出力したり、エラーが発生したりする可能性があります。

## 方法
ディレクトリの存在を確認するには、標準ライブラリの`<filesystem>`を使用します。具体的なコーディング例を以下に示します。

```C++
#include <iostream>
#include <filesystem>
namespace fs = std::filesystem;
int main() {
    std::string directory_path = "path/to/directory";
    if (fs::exists(directory_path)) {
        std::cout << "Directory exists!" << std::endl;
    } else {
        std::cout << "Directory does not exist." << std::endl;;
    }
}
```

上記のコードでは、`exists()`関数を使用して指定したディレクトリが存在するかどうかを判定しています。もし存在する場合は"Directory exists!"と出力され、存在しない場合は"Directory does not exist."と出力されます。

また、`<filesystem>`ライブラリにはさまざまなディレクトリ操作の関数が用意されており、さらに詳細なチェックや操作が可能です。

## 深堀り
ディレクトリの存在を確認する際に、重要なポイントとなるのがパスの指定方法です。上記のコードではパスとして文字列を使用しましたが、ファイルシステムによってはパスの表記方法が異なるため、注意が必要です。Windowsではバックスラッシュ`\`を使用し、Linuxではスラッシュ`/`を使用します。

また、`<filesystem>`ライブラリはC++17から導入された機能のため、古いバージョンのC++では使用することができません。その場合は代替となるライブラリを使用するか、自分でディレクトリの存在をチェックする関数を作成する必要があります。

## 同様に参考になる記事
- [プログラミング初心者が知るべきC++の基礎知識](https://qiita.com/chiyoyo/items/8079102a894c85055686)

## 参考リンク
- [C++リファレンス - <filesystem>](https://cpprefjp.github.io/reference/filesystem.html)
- [C++17 filesystemライブラリの紹介](https://cpprefjp.github.io/blog/2017/09/29/cpp17filesystem)
- [GNU Cライブラリ - ディレクトリの参照](https://www.gnu.org/software/libc/manual/html_node/Simple-Directory-Lister.html)