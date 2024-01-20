---
title:                "텍스트 파일 작성하기"
html_title:           "Elixir: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

무슨 & 왜?
새로운 파일을 만들기 위해 텍스트 파일을 작성하는 것은 매우 중요합니다. 프로그래머들은 이를 통해 데이터를 저장하고 읽을 수 있으며, 코드를 조직화하고 코드를 공유하는 데 도움이 됩니다.

어떻게:
```Elixir
# 새로운 텍스트 파일 만들기
File.write("new_file.txt", "Hello world!")

# 기존 파일에 내용 추가하기
File.append("existing_file.txt", "This is a new line!")

# 파일에서 데이터 읽어오기
File.read("existing_file.txt")

# 파일의 크기 확인하기
File.stat("existing_file.txt").size 
``` 

심층 분석:
- 텍스트 파일은 컴퓨터에서 문자열을 저장하는 데 사용됩니다.
- 다른 프로그래밍 언어에서도 파일을 작성할 수 있지만, Elixir에서는 강력한 파일 관리 기능이 제공됩니다.
- 파일을 작성할 때, 파일 이름과 쓸 내용을 지정하고, 기존 파일에 내용을 추가할 때는 파일 이름과 새로 추가할 내용을 지정합니다.
- 파일 크기를 확인할 때에는 File.stat() 함수를 사용하고 해당 파일의 속성을 확인할 수 있습니다.

관련 자료:
- [Elixir 공식 문서 - 파일](https://hexdocs.pm/elixir/File.html)