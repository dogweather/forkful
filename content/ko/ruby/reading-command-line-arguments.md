---
title:                "명령 줄 인수 읽기"
html_title:           "Ruby: 명령 줄 인수 읽기"
simple_title:         "명령 줄 인수 읽기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

Cmdline 인자를 읽는 것은 프로그래머들이 명령줄에서 사용자 입력을 가져오는 것입니다. 이는 사용자의 텍스트 기반 입력을 받는 것으로, 프로그램에 유용한 유연성과 사용자 정의를 제공합니다.

## 어떻게:

우선, `$ ruby myscript.rb arg1 arg2`와 같은 명령을 사용하여 Ruby 스크립트 파일을 실행합니다. 그런 다음 아래를 따라하면 쉽게 사용할 수 있습니다.

```Ruby
# myscript.rb
puts ARGV[0] # 출력: arg1
puts ARGV[1] # 출력: arg2
```

## 깊게 들어가기:

Cmdline 인자를 읽는 것은 기본적으로 프로그램의 명령줄 인자를 수집하는 것입니다. 보통 이것은 `ARGV`라는 Ruby 전역 변수를 통해 이뤄집니다. `ARGV`의 각 항목은 사용자가 제공한 명령줄 인자로 채워집니다. 따라서 `$ ruby myscript.rb arg1 arg2`를 실행하면 `ARGV`에는 `["arg1", "arg2"]`가 할당됩니다.

이전에`$ ruby myscript.rb`와 같이 실행했다면 `[]`가 할당됩니다. 즉, 사용자 입력이 없다는 것을 의미합니다.

이전에도 언급한 것처럼, cmdline 인자를 읽는 것은 프로그램에 유용한 유연성과 사용자 정의를 제공하는데에 매우 중요합니다. 프로그램을 실행할 때 다양한 인자를 제공함으로써 다른 데이터를 처리할 수 있게됩니다.

## 더 알아보기:

- 더 많은 방법으로 시스템 인풋을 처리하기 위해 [Ruby Docs](https://ruby-doc.org/core-2.7.1/ARGF.html)를 확인하세요.
- 명령줄 인자를 처리하는데 사용되는 다른 언어나 도구들을 알고 싶다면 [Command Line Parameters](https://en.wikipedia.org/wiki/Command-line_interface#Arguments)를 참조하세요.
- Joval L'Ecluse의 [Ruby (절차적) 프로그래밍할 때 CmdLine에서 사용자 인풋을 가져오기](https://joval.dev/ruby_standard_input)를 읽어보세요.