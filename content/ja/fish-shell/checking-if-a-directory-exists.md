---
title:    "Fish Shell: ディレクトリが存在するかどうかをチェックする"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜディレクトリの存在を確認するのか

ディレクトリを所有するかどうかを確認することは、ファイルやスクリプトを作成する上で非常に便利です。例えば、特定の場所にファイルを保存する前に、そのディレクトリが存在するかどうかを確認することができます。また、プログラムやスクリプトの実行時に、必要なディレクトリが存在しない場合は、それを作成することもできます。このように、ディレクトリの存在を確認することで、ファイルやスクリプトの作成や実行時のエラーを防ぐことができます。

## 方法

ディレクトリの存在を確認する方法は、シェルスクリプトで非常に簡単です。Fish Shellを使用して、以下のようにコマンドを入力します。

```Fish Shell
if test -d [ディレクトリ名]
  echo "ディレクトリは存在します"
else
  echo "ディレクトリは存在しません"
end
```

上記のコマンドを入力すると、指定したディレクトリが存在するかどうかに応じて、それに応じたメッセージが表示されます。

## 深い調査

ディレクトリを確認するためのコマンドは、実際には一連のステップを実行しています。まず、「test -d」コマンドを使用して、指定されたディレクトリが実際に存在するかどうかを確認します。存在する場合は「true」、存在しない場合は「false」を返します。そして、この結果をif文で評価し、存在する場合は「echo」コマンドでメッセージを表示します。これにより、ディレクトリの存在を確認することができます。

## 参考リンク

本記事では、Fish Shellを使用してディレクトリの存在を確認する方法を説明しました。より詳しい情報が必要な場合は、以下のリンクを参考にしてください。

- [Fish Shell公式ドキュメント](https://fishshell.com/docs/current/index.html)
- [Linuxでディレクトリが存在するかどうかを確認する方法](https://www.linuxtechi.com/check-directory-exists-not-linux/)
- [Fish Shellを使ってみよう](https://gemechu.me/using-fish-shell-works-kubernetes)
 
## 他に見る

 - [Fish Shellを使ったファイルの作業について学ぶ](https://www.digitalocean.com/community/tutorials/work-with-files-in-fish-shell)
 - [フロー制御を学ぶためのFish Shellチュートリアル](https://dev.to/itaiferber/flow-control-in-fish-shell-3ko7)
 - [Fish Shellでのディレクトリの扱い方について知る](https://fishshell.com/docs/current/tutorial.html#tut_the_pwd_and_cd_commands)