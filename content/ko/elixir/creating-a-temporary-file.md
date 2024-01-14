---
title:                "Elixir: 임시 파일 만들기"
simple_title:         "임시 파일 만들기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

왜: 한국어: 임시 파일을 만드는 것이 왜 중요한지에 대해 이야기해 봅시다.
임시 파일을 만들 때마다 새로운 파일 이름이 필요한 경우가 있습니다. 이것은 파일을 올바르게 추척하고 관리할 수 있게 해 줍니다.

# 왜?

임시 파일을 만드는 것은 파일을 관리할 때 중요한 요소입니다. 일시적으로 사용되는 파일을 따로 관리하고 추적하기 위해 임시 파일을 만들 수 있습니다. 매번 다른 파일 이름을 사용함으로써 파일을 구별하기 쉽게 하고, 파일이 필요 없어졌을 때 임시 파일을 삭제하는 것으로 디스크 공간을 정리할 수 있습니다.

# 사용 방법

임시 파일을 만들기 위해서는 `File` 모듈의 `temp_dir/0` 함수를 사용합니다. 이 함수는 시스템의 임시 디렉토리를 나타내는 경로를 반환합니다. 그리고 `Path` 모듈의 `join/2` 함수를 사용하여 임시 디렉토리와 파일 이름을 결합하여 새로운 임시 파일을 생성할 수 있습니다.

```Elixir
temp_dir = File.temp_dir()
tmp_file = Path.join(temp_dir, "my_temp_file.txt")

# 임시 파일 생성
File.write(tmp_file, "hello world")

# 임시 파일 삭제
File.rm(tmp_file)
```

위의 코드 예시에서는 `temp_dir/0` 함수가 임시 파일을 만들기 위한 기본 디렉토리 경로를 제공하고, `Path.join/2` 함수가 디렉토리 경로와 파일 이름을 결합하여 임시 파일의 전체 경로를 생성합니다. 그리고 `File.write/2` 함수를 사용하여 임시 파일에 데이터를 작성하고, `File.rm/1` 함수를 사용하여 임시 파일을 삭제합니다.

# 깊이 들어가기

`temp_dir/0` 함수는 운영 체제 마다 다른 임시 디렉토리를 반환할 수도 있습니다. 예를 들어, Linux 시스템에서는 `/tmp` 디렉토리를 반환하고, MacOS와 Windows 시스템에서는 각각 `/var/tmp`와 `%APPDATA%/Local/Temp` 디렉토리를 반환합니다. 이러한 차이 때문에 코드가 다른 계정이나 시스템에서 실행될 때 문제가 발생할 수 있습니다. 따라서 언제나 `temp_dir/0` 함수를 호출하기 전에 반환 값을 확인하는 것이 좋습니다.

# 알아보기

자신만의 임시 파일 관리 시스템을 만들기 위해 다양한 함수와 모듈을 활용할 수 있습니다. `Dir` 모듈을 사용하여 디렉토리 내의 파일 목록을 확인하고, `Path.wildcard/1` 함수를 사용하여 특정 패턴과 일치하는 파일을 검색할 수도 있습니다. 또한 `File` 모듈의 다른 함수들을 사용하여 파일을 읽고 쓰는 방법을 익힐 수 있습니다.

## 알아보기

- [Elixir 공식 문서](https://elixir-lang.org/docs.html)
- [Dir 모듈 문서](https://hexdocs.pm/elixir/Dir.html)
- [Path 모듈 문서](https://hexdocs.pm/elixir/Path.html)
- [File 모듈 문서](https://hexdocs.pm/elixir/File.html)
- [Elixir Cookbook - 임시 파일 만들기](https://elixirschool.com/lessons/advanced/files/#making-temporary-files)