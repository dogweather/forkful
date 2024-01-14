---
title:    "Elixir: 디렉토리가 존재하는지 확인하는 방법"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜?

디렉토리가 존재하는지 확인하는 것의 중요성은 여러 가지 이유로 나타납니다. 예를 들어, 해당 디렉토리가 존재하지 않는 경우, 프로그램이 오동작하거나 예상치 못한 오류가 발생할 수 있습니다. 또한 특정 디렉토리가 존재하지 않는 경우, 그 안에 있는 파일을 참조하려 할 때 오류가 발생할 수 있습니다. 따라서 디렉토리의 존재 여부를 확인하는 것은 소프트웨어 개발에서 중요한 단계 중 하나입니다.

## 방법

먼저 `Path` 모듈을 `import`하여 디렉토리 경로를 다룰 수 있도록 합니다. 다음으로, `File.stat/1` 함수를 사용하여 디렉토리의 정보를 가져옵니다. `File.stat/1` 함수는 주어진 파일의 상태를 나타내는 `{:ok, file_info}` 또는 `:error`를 반환합니다. 이 때 `:error`가 반환되는 경우, 해당 디렉토리가 존재하지 않는 것입니다. 즉, `|> case do` 블록을 사용하여 해당 경우에 대한 처리를 해주면 됩니다.

```Elixir
import Path
path = Path.join(["/my/directory", "subfolder"])
File.stat(path)
|> case do
  {:ok, file_info} -> IO.puts("디렉토리가 존재합니다.")
  :error -> IO.puts("해당 디렉토리는 존재하지 않습니다.")
end
```

위 예제에서 `Path.join/1` 함수를 사용하여 디렉토리 경로를 조합했지만 `Path.expand/1` 함수를 사용하면 현재 디렉토리에 대한 경로를 반환받을 수 있습니다. 코드의 다른 부분은 동일하게 작성하면 됩니다.

```Elixir
path = Path.expand("subfolder")
```

또한, `File.exists?/1` 함수를 사용하면 파일이나 디렉토리의 존재 여부를 간단히 확인할 수 있습니다.

```Elixir
File.exists?(path)
```

위의 예제에서 `File.stat/1` 함수를 사용하는 것보다 `File.exists?/1` 함수를 사용하는 것이 좀 더 편리하고 간단합니다.

## 심층 분석

디렉토리의 존재 여부를 확인하는 것은 운영 체제에 따라 처리 방식이 다릅니다. 일반적으로, 해당 디렉토리의 `inode` 값을 확인하는 것으로 구현됩니다. 이러한 처리 방식은 운영 체제나 파일 시스템의 구조에 맞춰져 있기 때문에 일반적으로 속도가 빠르고 안전합니다.

## 참고

- [Elixir 공식 문서 - Path 모듈](https://hexdocs.pm/elixir/Path.html)
- [Elixir 공식 문서 - File 모듈](https://hexdocs.pm/elixir/File.html)
- [Devhints.io - Elixir Cheat Sheet](https://devhints.io/elixir)