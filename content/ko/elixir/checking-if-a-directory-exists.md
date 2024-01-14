---
title:    "Elixir: 디렉토리가 존재하는지 확인하기"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

Korean Translation:

## 왜
디렉토리가 존재하는지 확인하는 것이 왜 중요한지에 대해 1-2 문장으로 설명합니다.

## 어떻게
"```Elixir ...```" 코드 블록 내부에서 코딩 예제와 샘플 출력을 사용하여 설명합니다.

```Elixir
defp check_directory_exists(path) do
  if File.exists?(path) do
    IO.puts("Directory exists.")
  else
    IO.puts("Directory does not exist.")
  end
end

check_directory_exists("./documents")
```

출력:
```
Directory exists.
```

## 더 깊게
디렉토리가 존재하는지 확인하는 방법에 대해 더 깊이 알아봅니다. 이 과정에서 디렉토리를 검색하기 위해 사용되는 함수와 옵션에 대해 자세히 설명합니다.

## 참고자료
- https://elixirschool.com/lessons/basics/flow-control/
- https://elixir-lang.org/getting-started/basic-operators.html#relational-operators
- https://hexdocs.pm/elixir/File.html#exists?/1