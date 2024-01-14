---
title:                "Elixir: 디렉토리가 존재하는지 확인하기"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

디렉토리가 존재하는지 확인하는 것이 왜 필요한지 궁금하신가요? 이 글에서는 디렉토리의 존재 여부를 확인하는 방법과 그에 대한 깊은 이해를 살펴보겠습니다.

## 방법

디렉토리가 존재하는지 확인하는 가장 간단한 방법은 `File.exists?/1` 함수를 사용하는 것입니다. 아래는 이 함수를 사용한 예시 코드입니다.

```Elixir
File.exists?("/home/username/Documents")
```

위 코드는 디렉토리 경로가 존재하는지에 따라 `true` 또는 `false`를 반환합니다. 만약 디렉토리가 존재하지 않는다면 `false`를 반환합니다.

더 나은 방법은 `File.stat/1` 함수를 사용하는 것입니다. 이 함수는 디렉토리의 상태를 나타내는 `:file` 레코드를 반환합니다.

```Elixir
File.stat("/home/username/Documents").file
```

위 코드는 다음과 같은 결과를 반환할 것입니다.

```
%File.Stat{
  access: :read_write,
  bytes: 4096,
  ctime: 1583972242,
  gid: 1000,
  inode: 3853673,
  mode: [directory: true, readable: true, writable: true],
  mtime: 1583972239,
  nlinks: 2,
  size: 4096,
  type: :regular,
  uid: 1000
}
```

여기서 `type` 값이 `:directory` 인 것을 확인할 수 있습니다.

## 깊이 들어가기

Elixir의 `File` 모듈에는 디렉토리 관련 함수를 사용할 수 있는 다양한 옵션이 있습니다. 예를 들어, `File.ls!/1` 함수를 통해 디렉토리 내의 파일 및 폴더 목록을 얻을 수 있습니다. 또한 `File.mkdir_p/1` 함수를 사용하면 디렉토리를 생성하는 것도 가능합니다.

하지만 `File.exists?/1` 함수는 해당 디렉토리가 실제로 존재하는지 확인하는 가장 간단하고 신뢰할 수 있는 방법입니다.

## 참고

- [Elixir - File 모듈 공식 문서 (한국어)](https://hexdocs.pm/elixir/File.html)
- [How to check if a file exists in Elixir](https://www.thesoftwaresimpleton.com/blog/2018/08/06/how-to-check-if-a-file-exists-in-elixir/)
- [Elixir Tip: Check if file/folder exists before working with it](https://medium.com/elixir-digest/elixir-tip-check-if-file-folder-exists-before-working-with-it-a71d263cb1b2)