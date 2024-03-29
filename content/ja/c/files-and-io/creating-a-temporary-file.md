---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:55:39.876707-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.821514-06:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
---

{{< edit_this_page >}}

## 何となぜ？
C言語で一時ファイルを作成することは、短期間使用するために生成されるファイルであり、通常はデータ処理やストレージのスクラッチスペースとして使われます。プログラマーは、プログラムの永続的なストレージに影響を与えずに一時的なデータを管理するため、または使用後に機密データが消去されることを保証するためにこれを行います。

## 方法：
C言語で一時ファイルを作成するためには、`tmpfile()`や`mkstemp()`などの関数を利用できます。

**`tmpfile()`の使用：** この関数は、プログラムが終了する際やファイルが閉じられる際に自動的に削除される一意の一時ファイルを作成します。

```c
#include <stdio.h>

int main() {
    FILE *temp = tmpfile();
    if (temp == NULL) {
        perror("一時ファイルの作成に失敗");
        return 1;
    }

    // 一時ファイルへのデータ書き込み
    fputs("これはテストです。\n", temp);

    // 書き込んだ内容をリワインドして読み取る
    rewind(temp);
    char buffer[1024];
    while (fgets(buffer, sizeof(buffer), temp) != NULL) {
        printf("%s", buffer);
    }

    // 閉じるかプログラムが終了すると自動的に削除
    fclose(temp);

    return 0;
}
```
**サンプル出力：**
```
これはテストです。
```

**`mkstemp()`の使用：** 一時ファイルの場所やその権限についてより多くの制御を提供します。`XXXXXX`で終わるテンプレート文字列が必要で、それを一意のシーケンスに置き換えて名前の衝突を防ぎます。

```c
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>

int main() {
    char template[] = "/tmp/mytemp-XXXXXX";
    int fd = mkstemp(template);

    if (fd == -1) {
        perror("一時ファイルの作成に失敗");
        return 1;
    }
    
    printf("一時ファイルが作成されました: %s\n", template);

    // mkstemp()で作成された一時ファイルは手動で削除する必要があります
    unlink(template);

    close(fd);
    return 0;
}
```
**サンプル出力：**
```
一時ファイルが作成されました: /tmp/mytemp-abc123
```

## 深い潜水
一時ファイルの概念はCに特有のものではなく、その実用性のために多くのプログラミング環境で一般的な機能です。ISO C標準で規格化された`tmpfile()`関数は、標準ディレクトリに一意の名前のファイルを作成しますが、その存在ははかなく、セキュアまたは一時的な操作に理想的です。

`tmpfile()`の特筆すべき制限の一つは、デフォルトの一時ディレクトリに依存していることで、特に権限やセキュリティの観点からすべてのアプリケーションに適しているわけではありません。対照的に、`mkstemp()`はディレクトリを指定でき、提供されたテンプレート文字列を変更することで、一意のファイル名のセキュアなファイル作成を保証し、手動ファイル管理の犠牲を払ってより多用途なソリューションを提供します。

しかし、一時ファイルの作成は、適切に扱われない場合には、レースコンディションなどのセキュリティ脆弱性を導入する可能性があります。たとえば、`tmpfile()`と`mkstemp()`は、セキュアな一時ファイル作成の異なる側面（自動削除とセキュアな名前生成、それぞれ）に対処しますが、どちらも万能薬ではありません。開発者は、一時ファイルによって導入される可能性のある脆弱性を含む、アプリケーションのセキュリティ要件の詳細を考慮し、これらの関数が提供するものを超えて追加の保護措置を実装する必要があります。

プログラミングの広い景観で、一時データ処理においてパフォーマンスやセキュリティを向上させるために代替手段（例えば、動的データ構造やメモリマップトファイルを使用する）が提案されています。それにもかかわらず、物理的な一時ファイルは大規模なデータセットを扱う場合やプロセス間通信が関与する場合など、多くのシナリオで極めて重要なツールのままです。
