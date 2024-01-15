---
title:                "新しいプロジェクトを開始する"
html_title:           "Java: 新しいプロジェクトを開始する"
simple_title:         "新しいプロジェクトを開始する"
programming_language: "Java"
category:             "Java"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/starting-a-new-project.md"
---

{{< edit_this_page >}}

（なぜ新しいプロジェクトをはじめるのか）
新しいプロジェクトを始める理由はたくさんありますが、主な理由は、新しいスキルを習得したり、自分のアイデアを具体化したりするためです。

（始め方）
新しいJavaプロジェクトを始める方法はとても簡単です！まずは適当なIDEをダウンロードし、そこで新しいJavaプロジェクトを作成します。そして、お好みのコードを書き、実行ボタンを押して結果を確認しましょう。

```Java
public class NewProject {
  public static void main(String[] args) {
    System.out.println("Hello World!");
  }
}
```

``` bash
$ javac NewProject.java
$ java NewProject
```

（今回のテーマについて）
新しいプロジェクトを始めるのはワクワクするものですが、実際には少し手間がかかるかもしれません。まずはプロジェクトの目的や範囲を明確にし、必要なライブラリーやツールを選定しましょう。また、コーディング規約やバージョン管理についても検討することが重要です。

## 他に見るべきもの
- Javaプロジェクト管理の基本: https://www.geeksforgeeks.org/manage-java-projects-with-maven/

- Javaコーディングスタイルガイド: https://google.github.io/styleguide/javaguide.html

- プロジェクト管理ツールを使った開発のコツ: https://dzone.com/articles/scrum-made-simple-with-agile-project-management-too