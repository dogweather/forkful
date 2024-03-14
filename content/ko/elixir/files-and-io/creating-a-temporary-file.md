---
date: 2024-01-20 17:39:51.207684-07:00
description: "\uC784\uC2DC \uD30C\uC77C\uC744 \uB9CC\uB4DC\uB294 \uAC83\uC740 \uC2DC\
  \uC2A4\uD15C\uC5D0\uC11C \uC784\uC2DC \uB370\uC774\uD130\uB97C \uC800\uC7A5\uD560\
  \ \uB54C \uC0AC\uC6A9\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC774\
  \ \uC774\uB97C \uC0AC\uC6A9\uD558\uB294 \uC774\uC720\uB294 \uB300\uCCB4\uB85C \uB370\
  \uC774\uD130\uB97C \uC77C\uC2DC\uC801\uC73C\uB85C \uCC98\uB9AC\uD558\uAC70\uB098\
  , \uD14C\uC2A4\uD2B8\uB97C \uC704\uD55C \uBAA9\uC801 \uB54C\uBB38\uC785\uB2C8\uB2E4\
  ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:54.749957-06:00'
model: gpt-4-1106-preview
summary: "\uC784\uC2DC \uD30C\uC77C\uC744 \uB9CC\uB4DC\uB294 \uAC83\uC740 \uC2DC\uC2A4\
  \uD15C\uC5D0\uC11C \uC784\uC2DC \uB370\uC774\uD130\uB97C \uC800\uC7A5\uD560 \uB54C\
  \ \uC0AC\uC6A9\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC774 \uC774\
  \uB97C \uC0AC\uC6A9\uD558\uB294 \uC774\uC720\uB294 \uB300\uCCB4\uB85C \uB370\uC774\
  \uD130\uB97C \uC77C\uC2DC\uC801\uC73C\uB85C \uCC98\uB9AC\uD558\uAC70\uB098, \uD14C\
  \uC2A4\uD2B8\uB97C \uC704\uD55C \uBAA9\uC801 \uB54C\uBB38\uC785\uB2C8\uB2E4."
title: "\uC784\uC2DC \uD30C\uC77C \uC0DD\uC131\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
임시 파일을 만드는 것은 시스템에서 임시 데이터를 저장할 때 사용합니다. 프로그래머들이 이를 사용하는 이유는 대체로 데이터를 일시적으로 처리하거나, 테스트를 위한 목적 때문입니다.

## How to: (방법)
Elixir에서 임시 파일을 다루려면 `:os` 모듈과 `File` 모듈을 사용할 수 있습니다. 아래 예제를 보세요.

```elixir
# 임시 디렉터리에 임시 파일 이름을 생성합니다.
temp_path = :os.tmpdir() |> Path.join("my_temp_file_#{:os.timestamp()}")
# 임시 파일 안에 텍스트를 씁니다.
File.write!(temp_path, "임시 파일에 저장할 텍스트입니다.")

# 파일이 제대로 생성되었는지 확인합니다.
{:ok, content} = File.read(temp_path)
IO.puts(content)  # 출력: 임시 파일에 저장할 텍스트입니다.

# 사용 후 파일을 삭제합니다.
File.rm(temp_path)
```

## Deep Dive (심층 탐구)
임시 파일은 프로그램이 실행되는 동안에만 필요하고, 끝나면 안전하게 삭제되어야 합니다. 과거에는 `tmp` 디렉터리에 직접 파일을 만들기도 했지만, 이는 보안 문제를 낳을 수 있습니다 (예: 경로 순회 공격). Elixir에서는 `:os.tmpdir()` 함수를 사용해 플랫폼 독립적으로 안전한 임시 패스를 구할 수 있습니다. 대안으로 NIF(Native Implemented Function) 라이브러리나 Erlang의 `:file` 모듈을 사용하여 더 깊은 기능을 구현할 수도 있습니다. 하지만 일반적인 사용에는 기본 `File` 모듈이 충분합니다.

## See Also (참고 자료)
- Elixir의 [`File` 모듈 문서](https://hexdocs.pm/elixir/File.html)
- Erlang의 [`:file` 모듈 문서](http://erlang.org/doc/man/file.html)
- [`:os` 모듈 문서](http://erlang.org/doc/man/os.html)
