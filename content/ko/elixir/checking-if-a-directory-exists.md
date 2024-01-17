---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "Elixir: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 뭐고 왜?

디렉토리가 존재하는지 확인하는 것이 무엇인지 그리고 왜 프로그래머들이 이를 하는지 이해해 보겠습니다.

디렉토리가 단순히 파일이나 다른 디렉토리를 담고 있는 공간이라는 것을 알 수 있습니다. 프로그래머들은 디렉토리에 대한 존재 여부를 확인하기 위해 일반적으로 이를 사용합니다. 이것은 작업 중에 오류를 방지하고 코드를 실행하기 전에 적절한 처리를 하기 위함입니다.

# 하는 법:

```Elixir
File.exists?("/Users/username/Documents")
# => true
```

위의 코드는 "/Users/username/Documents" 디렉토리가 존재하는지 확인합니다. 결과는 `true`로 나올 것입니다.

```Elixir
File.exists?("/Users/username/Desktop/Nonexistent")
# => false
```

반면에, 위의 코드는 "/Users/username/Desktop/Nonexistent" 디렉토리가 존재하는지 확인합니다. 결과는 `false`로 나올 것입니다.

# 깊이 살펴보기:

(1) 디렉토리의 존재 여부를 확인하는 것은 오래 된 방법입니다. 예를 들어, C언어에서는 stat() 함수를 사용하여 이를 구현할 수 있습니다. 하지만 Elixir에서는 File 모듈의 exists? 함수를 사용하는 것이 더 쉽고 간편합니다.

(2) 파일 시스템을 조작하는 다른 방법으로는 제거, 복사, 이름 변경 등이 있습니다. 이때에도 디렉토리의 존재여부를 확인하는 것은 중요합니다. 예를 들어, 디렉토리가 존재하지 않는데 삭제나 복사를 시도하면 오류가 발생합니다. 이를 미연에 방지할 수 있습니다.

(3) File 모듈의 exists? 함수는 Erlang의 filelib 모듈의 함수를 사용하여 구현됩니다. Elixir에서는 이를 더욱 쉽고 간편하게 사용할 수 있도록 API를 제공합니다.

# 더 볼거리:

- [Elixir documentation for File.exists? function](https://hexdocs.pm/elixir/File.html#exists?/1)
- [Erlang documentation for filelib module](http://erlang.org/doc/man/filelib.html)