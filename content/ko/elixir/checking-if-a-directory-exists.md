---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "Elixir: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜?

누군가가 디렉토리가 존재하는지 확인하는 것에 참여하는 이유는 보통 해당 디렉토리가 애플리케이션의 일부이기 때문입니다.

## 방법

입력된 경로에서 디렉토리가 존재하는지 확인하는 방법을 아래의 예제와 출력 결과를 포함한 ```Elixir ... ``` 코드 블록으로 설명하겠습니다.

```Elixir
{:ok, dir} = File.cwd() # 현재 디렉토리 경로 가져오기
File.exists?(dir) # 디렉토리의 존재 확인
```

출력 결과:

```
true
```

## 더 깊이 들어가기

디렉토리의 존재 여부를 확인하는 방법은 보통 File 모듈의 exists? 함수를 사용합니다. 이 함수는 해당 경로가 존재하는지 여부를 판단하는 단순한 절차를 따릅니다. 따라서 만약 응용 프로그램에서 사용되는 디렉토리가 바뀐다면, 코드를 수정하지 않고도 새로운 디렉토리의 존재 여부를 확인할 수 있어 유용합니다.

## 관련 링크

- [File 모듈 문서](https://hexdocs.pm/elixir/File.html)
- [File.exists? 함수 문서](https://hexdocs.pm/elixir/File.html#exists?/2)
- [Elixir 디렉토리 관련 자료](https://elixir-lang.org/getting-started/file-system.html)