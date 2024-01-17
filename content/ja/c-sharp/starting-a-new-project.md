---
title:                "新しいプロジェクトを始める"
html_title:           "C#: 新しいプロジェクトを始める"
simple_title:         "新しいプロジェクトを始める"
programming_language: "C#"
category:             "C#"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/starting-a-new-project.md"
---

{{< edit_this_page >}}

## なに？そしてなぜ？

新しいプロジェクトを始めるというのは、プログラマーが新しいアプリケーションやプログラムを開発することを意味します。プログラマーは、新しいアイデアを実現するために、新しいプロジェクトを始めます。

## 方法：

```c#
using System; // C#の基本ライブラリをインポートする

class Program
{
    static void Main()
    {
        // 新しいプロジェクトを始めるには、まずプロジェクトフォルダーを作成します
        string projectFolder = "新しいプロジェクト";
        // プロジェクトフォルダーを作成するコマンド
        string createFolderCommand = $"mkdir {projectFolder}";
        // コマンドを実行するためのProcessを作成
        System.Diagnostics.Process process = new System.Diagnostics.Process();
        process.StartInfo.FileName = "cmd.exe"; // コマンドプロンプトを実行
        process.StartInfo.Arguments = $"/C {createFolderCommand}"; // コマンドを実行するための引数を設定
        process.Start(); // コマンドを実行する
        process.WaitForExit(); // コマンドの終了を待つ

        // プロジェクトフォルダー内にC#のプロジェクトを作成するためのコマンドを作成
        string createProjectCommand = $"dotnet new console --output {projectFolder}";
        
        process = new System.Diagnostics.Process();
        process.StartInfo.FileName = "cmd.exe"; // コマンドプロンプトを実行
        process.StartInfo.Arguments = $"/C {createProjectCommand}"; // コマンドを実行するための引数を設定
        process.Start(); // コマンドを実行する
        process.WaitForExit(); // コマンドの終了を待つ
    }
}
```
上記のプログラムを実行すると、"新しいプロジェクト"という名前のフォルダーが作成され、その中にC#のプロジェクトが作成されます。

## 詳細

プログラミングの歴史において、新しいプロジェクトを始めることは常に重要な一歩でした。新しいテクノロジーが登場するたびに、プログラマーたちはそれを活用し、新しいアプリケーションやプログラムを開発してきました。

C#には、プロジェクトを始める方法としてdotnetコマンドの他にも、Visual Studioなどの統合開発環境（IDE）を使う方法もあります。また、他のプログラミング言語にも同じようなプロジェクト作成の方法が存在します。

新しいプロジェクトを始める際には、様々な要素を考慮する必要があります。例えば、プロジェクトの目的や機能、必要なライブラリやツール、開発するためのスケジュールなどです。

## 関連リンク

- [dotnetコマンドによるプロジェクト作成の詳細](https://docs.microsoft.com/ja-jp/dotnet/core/tools/dotnet-new)
- [Visual Studioを使ったC#のプロジェクト作成の方法](https://docs.microsoft.com/ja-jp/visualstudio/get-started/csharp/tutorial-console?view=vs-2019)
- [git initコマンドによるプロジェクトのバージョン管理](https://git-scm.com/docs/git-init)