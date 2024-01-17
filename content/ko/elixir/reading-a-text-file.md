---
title:                "텍스트 파일 읽기"
html_title:           "Elixir: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?
텍스트 파일을 읽는 것은 단순히 파일의 내용을 화면에 출력하는 것입니다. 프로그래머들은 이 작업을 수행하여 텍스트 파일에 저장된 정보를 애플리케이션의 다른 부분과 연결하거나 가공할 수 있습니다.

## 하는 법:
```Elixir
# 파일 경로를 지정
path = "test.txt"
# 파일 열기
file = File.open(path)
# 파일 내용 읽기
contents = IO.read(file)
# 출력
IO.puts(contents)
```
위의 코드를 실행하면 "test.txt" 파일의 내용이 화면에 출력될 것입니다.

## 깊이 파고들기:
- 텍스트 파일을 읽는 것은 컴퓨터의 초기부터 사용된 기본적인 작업 중 하나입니다.
- Elixir에서는 File 모듈을 사용하여 파일을 다룰 수 있습니다.
- 파일을 열 때는 반드시 경로를 지정해야 하며, 파일이 존재하지 않을 경우에는 오류가 발생할 수 있습니다.

## 관련 자료:
- [Elixir File 모듈 문서](https://hexdocs.pm/elixir/File.html)
- [Elixir File 모듈 예제](https://elixirschool.com/ko/lessons/basics/io-and-the-file-system/)
- [텍스트 파일을 읽는 다른 방법](https://medium.com/@funk3y/handling-text-file-in-elixir-6e49a7f908a1)