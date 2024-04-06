---
date: 2024-01-20 17:39:51.207684-07:00
description: "How to: (\uBC29\uBC95) Elixir\uC5D0\uC11C \uC784\uC2DC \uD30C\uC77C\uC744\
  \ \uB2E4\uB8E8\uB824\uBA74 `:os` \uBAA8\uB4C8\uACFC `File` \uBAA8\uB4C8\uC744 \uC0AC\
  \uC6A9\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC544\uB798 \uC608\uC81C\uB97C \uBCF4\
  \uC138\uC694."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.578405-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) Elixir\uC5D0\uC11C \uC784\uC2DC \uD30C\uC77C\uC744 \uB2E4\
  \uB8E8\uB824\uBA74 `:os` \uBAA8\uB4C8\uACFC `File` \uBAA8\uB4C8\uC744 \uC0AC\uC6A9\
  \uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."
title: "\uC784\uC2DC \uD30C\uC77C \uC0DD\uC131\uD558\uAE30"
weight: 21
---

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
