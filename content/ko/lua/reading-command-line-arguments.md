---
title:                "명령줄 인수 읽기"
html_title:           "Arduino: 명령줄 인수 읽기"
simple_title:         "명령줄 인수 읽기"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

커맨드 라인 인자를 읽는 것은 프로그램이 외부 입력을 받아 처리하는 방법입니다. 프로그래머는 사용자로부터 직접적인 입력을 받아 동적으로 프로그램을 제어하기 위해 이를 사용합니다.

## 어떻게:

Lua에서 커맨드 라인 인자를 읽는 방법은 매우 간단합니다. `arg`라는 전역 테이블에서 접근할 수 있습니다. 다음은 기본 예시입니다:

```Lua
-- 예제 프로그램: arg_test.lua
print(arg[0]) -- 프로그램 이름 출력
print(arg[1]) -- 첫 번째 인자 출력
print(arg[2]) -- 두 번째 인자 출력
```
이 프로그램을 "lua arg_test.lua hello world"라고 실행하면 다음과 같은 결과를 얻습니다:

```Lua
arg_test.lua
hello
world
```

## 깊이 알아보기:

Lua에는 오래전부터 `arg` 테이블이 존재했으며, 이것은 스크립트에 접근 가능한 가장 직접적인 방법입니다. `arg` 테이블 외에도 `io.read()`를 사용하여 표준 입력에서 데이터를 직접 읽는 방법도 있습니다. 하지만, 이를 사용하면 사용자가 콘솔에서 데이터를 직접 입력해야합니다.

직접 커맨드 라인 인자를 구현하는 것은 복잡하므로 Lua는 이를 간단하게 만들어줍니다. `arg` 테이블은 루아가 프로그램을 시작할 때 커맨드 라인 인자를 자동으로 채워주고, `arg[n]`은 `n`번째 인자를 나타냅니다. `n`이 0일 경우 프로그램의 이름을 반환합니다.

## 참고 자료:

더 많은 정보를 찾을 수 있는 몇 가지 링크를 제공합니다:
- Lua 공식 문서: [Lua Programming Guide](https://www.lua.org/pil/24.html)
- Lua 관련 자료 모음: [Lua-users tutorials](http://lua-users.org/wiki/TutorialDirectory)
- Lua에 대한 StackOverflow 질문: [How to handle command-line arguments](https://stackoverflow.com/questions/43574426/how-to-handle-the-command-line-argument-in-lua)