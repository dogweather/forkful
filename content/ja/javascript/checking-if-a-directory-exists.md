---
title:    "Javascript: ディレクトリが存在するかどうかを確認する"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# なぜディレクトリの存在をチェックする必要があるのか

Javascriptプログラミングをしていると、時にはファイルやディレクトリの存在をチェックする必要があります。これは、特定の処理を行う前に、必要なファイルやディレクトリが存在するかどうかを確認するためです。そこで、今回はディレクトリの存在をチェックする方法について解説します。

## 方法

Javascriptでディレクトリの存在をチェックするには、fsモジュールを使用します。まずは、fsモジュールをインポートします。次に、existsSync()メソッドを使用し、チェックしたいディレクトリのパスを引数として渡します。存在する場合はtrueを、存在しない場合はfalseを返します。

```Javascript
const fs = require('fs');

if(fs.existsSync('./myDirectory')){
  console.log('myDirectory exists');
} else {
  console.log('myDirectory does not exist');
}
```

上記のコードでは、myDirectoryという名前のディレクトリが存在するかどうかをチェックしています。もし存在する場合は、「myDirectory exists」というメッセージがコンソールに表示されます。存在しない場合は、「myDirectory does not exist」というメッセージが表示されます。

## ディープダイブ

さらに詳しく調べると、existsSync()メソッドは同期的な方法でディレクトリの存在をチェックします。つまり、コードの実行を一時停止してチェックを行います。そのため、存在するかどうかを確認するだけであれば便利ですが、複数のファイルやディレクトリをチェックする場合は非推奨です。

代わりに、exists()メソッドを使うことで非同期的にチェックを行うことができます。これは、コードの実行をブロックせずに処理を進めるため、複数のチェックを行う場合や大きなファイルを処理する場合にはより効率的です。

また、exists()メソッドを使用する場合はコールバック関数を指定する必要があります。コールバック関数には引数としてエラーと結果が渡されるため、返り値を取得するにはそれらを利用する必要があります。

```Javascript
const fs = require('fs');

fs.exists('./myDirectory', (err, result) => {
  if(err) {
    console.error(err);
  } else {
    if(result) {
      console.log('myDirectory exists');
    } else {
      console.log('myDirectory does not exist');
    }
  }
})
```

このように、exists()メソッドを使用すればより柔軟にディレクトリの存在をチェックすることができます。

# また見る

- [fsモジュールの公式ドキュメント](https://nodejs.org/api/fs.html)
- [exists() vs existsSync()の比較](https://stackoverflow.com/questions/4482686/check-synchronously-if-file-directory-exists-in-node-js)