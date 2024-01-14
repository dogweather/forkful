---
title:                "Elixir: 임시 파일 만들기"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜

임시 파일을 만드는 것에 참여하는 이유는 다양합니다. 주로 데이터를 생성하거나 저장하는데 유용합니다. 또한 프로그램 실행 중에 생성된 데이터를 보관하고 나중에 사용하기 위해서일 수도 있습니다.

## 만드는 방법

임시 파일을 만드는 방법은 매우 간단합니다.

```Elixir
# Elixir 코드로 임시 파일 생성
IO.puts "Creating temporary file..."

# 임시 파일 경로 지정
temp_file = File.temp_name()

# 임시 파일에 데이터 쓰기
File.write(temp_file, "This is a temporary file!")

# 임시 파일 읽기
data = File.read(temp_file)
IO.puts "Temporary file contents: #{data}"
```

위의 코드를 실행하면 `Creating temporary file...`과 `Temporary file contents: This is a temporary file!`가 출력됩니다. 임시 파일은 운영체제가 지정한 기본 임시 폴더에 생성되며, 데이터를 쓰고 나중에 파일을 닫으면 자동으로 삭제됩니다.

## 깊이 파고들기

임시 파일을 만드는 방법은 운영체제에 따라 다를 수 있지만, Elixir에서는 `File` 모듈을 사용하여 간단하게 임시 파일을 만들 수 있습니다. 이 모듈은 파일 작업에 유용한 함수를 제공하며, 임시 파일 생성 또한 가능합니다. 임시 파일을 생성할 때 파일명을 지정할 수도 있지만, 보통은 `File.temp_name()` 함수의 리턴 값인 임시 파일명을 사용합니다. 임시 파일 생성 시, 해당 파일의 사용 권한을 설정할 수도 있습니다. 기본 권한은 `644`로, 읽기와 쓰기 권한이 있는 파일입니다. 하지만 원하는 권한을 지정할 수도 있으며, 이를 통해 임시 파일의 보안을 강화할 수 있습니다.

## See Also

- [Elixir File 모듈 문서](https://hexdocs.pm/elixir/File.html)
- [Erlang 파일 관련 함수 문서](http://erlang.org/doc/man/file.html)
- [Linux 임시 파일 설명서](https://en.wikipedia.org/wiki/Temporary_file#Linux)