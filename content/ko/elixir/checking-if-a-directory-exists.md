---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "Bash: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 뭐 & 왜?

디렉토리 존재 확인은 파일 시스템에서 특정 디렉토리가 존재하는지 여부를 검사하는 것입니다. 프로그래머들은 파일에 쓰거나 읽기 전에 에러를 방지하기 위해 이 작업을 수행합니다.

## 방법:

Elixir에서 디렉토리의 존재 여부를 확인하려면 File 모듈 내의 `exists?/1` 함수를 사용할 수 있습니다. 

```Elixir
case File.ls("/your/directory/path") do
  {:ok, _files} -> 
    IO.puts("디렉토리가 존재합니다.")
  {:error, _reason} -> 
    IO.puts("디렉토리가 존재하지 않습니다.")
end
```
위의 코드는 파일의 목록을 가져오려고 시도하고, 실패하면 디렭토리가 존재하지 않는 것으로 판단합니다.

## 깊게 알아보기

체크의 수행이 역사적으로 프로그램 작성의 중요한 부분이었고, 디스크 공간 관리에서 흔히 볼 수 있는 작업입니다. Elixir가 첫 번째 버전에서 제공한 `File.exists?/1` 함수 외에도, 현재는 `File.ls/1`함수와 같은 대안이 있습니다.

`File.exists?/1`는 주어진 경로에 파일 또는 디렉토리가 있는지 확인합니다. 반면 `File.ls/1`는 특정 디렉토리 안의 모든 파일과 하위 디렉토리를 나열하려고 시도합니다.

## 참고:

더 많은 정보는 Elixir 공식 문서에서 찾을 수 있습니다.

- File 모듈: https://hexdocs.pm/elixir/File.html
- 에러 처리: https://elixirschool.com/ko/lessons/advanced/error-handling/