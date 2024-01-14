---
title:                "C++: ディレクトリが存在するかどうかをチェックする"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

＃＃なぜ？
コンピュータープログラミングにおいて、ファイルやフォルダーの存在をチェックすることは重要です。特に、ファイルを確実に読み込むために、事前にファイルが存在するかどうかを確認する必要があります。この記事では、ディレクトリが存在するかどうかをチェックする方法を紹介します。

＃＃方法
"```C++
#include <iostream>
#include <filesystem>
using namespace std;
namespace fs = std::filesystem;
int main()
{
    // チェックするディレクトリのパスを指定
    fs::path path = "C:\\Users\\User\\Documents\\Folder";
    // ディレクトリが存在するかをチェック
    if(fs::exists(path))
    {
        cout << "フォルダーが存在します。" << endl;
    }
    else
    {
        cout << "フォルダーが存在しません。" << endl;
    }
    // サブディレクトリも含めてチェックする場合はrecursiveオプションを指定
    if (fs::exists(path, fs::directory_options::skip_permission_denied | fs::directory_options::recursive))
    {
        cout << "サブディレクトリも含めてフォルダーが存在します。" << endl;
    }
    return 0;
}
```"
コード例を見るとわかるように、<code>std::filesystem</code>ライブラリを使用し、<code>fs::exists</code>関数を使用してディレクトリが存在するかどうかをチェックします。また、<code>recursive</code>オプションを使用することで、サブディレクトリも含めてチェックすることができます。

＃＃ディープダイブ
ファイルやフォルダーの存在をチェックする方法は、実際にはさまざまな手法があります。例えば、<code>stat</code>システムコールを使用したり、ファイルシステムへの直接アクセスを行ったりする方法もあります。しかし、<code>std::filesystem</code>ライブラリを使用することで、より簡単かつ安全にディレクトリの存在をチェックすることができます。

＃＃参考資料
- [C++でファイルやディレクトリの存在をチェックする方法](https://cpprefjp.github.io/reference/filesystem/exists.html)
- [std::filesystem：C ++ 17のファイルシステムライブラリ](https://cpprefjp.github.io/reference/filesystem.html)
- [ファイル/ディレクトリが存在するかチェックする方法を調べる](https://stackoverflow.com/questions/12774207/fastest-way-to-check-if-a-file-exist-using-standard-c-c11-c)
- [statシステムコールについての詳細](https://qiita.com/bamchoh/items/792196d1c8c689563d48)