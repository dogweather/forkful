---
title:    "Elixir: 텍스트 파일 작성하기"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# 왜

테스트 파일을 작성하는 것이 왜 유용한지 궁금하신가요? 이 블로그 포스트를 통해 Elixir 프로그래밍을 하기 위해 테스트 파일을 작성하는 이유를 알아보세요.

# 어떻게

테스트 파일을 작성하는 방법은 어려울 수 있지만, Elixir에서는 매우 간단합니다. 아래의 예제 코드와 출력 결과를 보고 따라해보세요.

```elixir
defmodule FileExample do
  def write_to_file(text) do
    File.write("example.txt", text)
  end
end

FileExample.write_to_file("Hello world!") # example.txt에 "Hello world!"를 작성합니다.
# :ok
```

위의 예제에서는 `defmodule`을 사용하여 모듈을 정의하고, `def`를 사용하여 함수를 정의합니다. `File.write` 함수를 사용하여 파일에 텍스트를 작성하고, `:ok`라는 출력 결과를 확인할 수 있습니다.

# 깊이 파고들기

테스트 파일을 작성하는 것도 중요하지만, 어떻게 파일을 읽고 수정할 수 있는지도 알아보는 것이 중요합니다. Elixir에서는 `File.read`와 `File.write` 함수 외에도 다양한 파일 관련 함수를 제공합니다. 또한 Elixir의 파일 시스템 모듈인 `File`과 `Path`에 대한 자세한 정보도 알아보세요.

# See Also

- [Elixir의 File 모듈에 대한 문서](https://hexdocs.pm/elixir/File.html)
- [Elixir의 Path 모듈에 대한 문서](https://hexdocs.pm/elixir/Path.html)
- [Elixir의 파일 관련 예제 코드](https://github.com/elixir-lang/elixir/tree/master/lib/elixir/lib/file)