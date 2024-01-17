---
title:                "프로그래밍에서 명령 줄 인수를 읽는 방법"
html_title:           "Elixir: 프로그래밍에서 명령 줄 인수를 읽는 방법"
simple_title:         "프로그래밍에서 명령 줄 인수를 읽는 방법"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 무엇과 왜?

커맨드 라인 인수를 읽는 것은 프로그래머들이 사용자로부터 입력받은 정보를 읽고, 해당 정보를 기반으로 프로그램을 실행시키기 위해 필요한 작업입니다. 이를테면, 사용자가 프로그램을 실행할 때 어떤 파일을 선택하거나 어떤 옵션을 설정하거나, 입력값을 전달할 때 이를 인식하여 프로그램이 이를 수행할 수 있도록 해줍니다.


# 어떻게:

커맨드 라인 인수를 읽기 위해서는 `System.argv` 함수를 사용하면 됩니다. 예시 코드는 다음과 같습니다:

```Elixir
# 커맨드 라인 인수를 리스트로 읽어옵니다.
arguments = System.argv

# 인수들을 모두 출력합니다.
IO.puts(arguments)
```

위 코드를 실행하면, 다음과 같은 결과를 볼 수 있습니다:

```Elixir
["my_program.ex", "file1.txt", "-a"]
```

위 예시 코드를 통해, 프로그램 파일의 이름과 더불어 사용자로부터 입력받은 파일 이름과 옵션 등을 읽을 수 있습니다.


# 더 깊이 들어가보기:

커맨드 라인 인수 읽기는 프로그래밍에서 자주 사용되는 작업 중 하나입니다. 이는 이전에도 다른 프로그래밍 언어에서도 사용이 가능했으며, `System.argv`는 Elixir에서 이 작업을 쉽게 처리할 수 있도록 만들어진 함수 중 하나입니다.

하지만 때로는, 커맨드 라인 인수를 직접 읽기보다는 입력된 인수들을 `Application.get_env`를 통해 읽어와서 사용하는 방법으로 대체하기도 합니다. 이는 코드를 조금 더 유연하게 만들어줄 수 있습니다.

커맨드 라인 인수를 읽어오는 방식은 운영체제에 따라 조금씩 다를 수 있으므로, 필요에 따라 이를 잘 활용하여 프로그램을 작성해야 합니다.


# 관련 링크:

- [Elixir 공식 문서 - System](https://hexdocs.pm/elixir/System.html#argv/0)
- [커맨드 라인 인수 읽기 예제 - Elixir School](https://elixirschool.com/lessons/basics/command-line-args/)