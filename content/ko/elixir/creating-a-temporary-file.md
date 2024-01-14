---
title:    "Elixir: 임시 파일 만들기"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜 만들어야 할까요?

임시 파일을 생성하는 이유는 주로 데이터를 일시적으로 저장하거나 프로그램 실행 중에 필요한 파일을 생성하기 위해서입니다. Elixir에서 임시 파일을 만드는 방법을 알아보겠습니다.

## 만드는 방법

임시 파일을 만들기 위해서는 `File` 모듈과 `Base` 모듈을 사용합니다. 아래는 `File.temp_dir!` 함수를 사용하여 임시 디렉토리를 생성한 후, 해당 위치에 임시 파일을 만드는 코드 예시입니다.

```Elixir
temp_dir = File.temp_dir!  # 임시 디렉토리 생성
file_path = "#{temp_dir}/my_temp_file.txt"  # 임시 파일 경로 지정

{:ok, file} = File.open(file_path, [:write, :utf8])  # 파일 열기

IO.write(file, "This is a temporary file.")  # 파일에 내용 쓰기

:ok = File.close(file)  # 파일 닫기

IO.puts("Temp file successfully created at #{file_path}")  # 임시 파일 생성 완료 메시지 출력
```

위 코드를 실행하면 현재 작업 디렉토리 내에 `my_temp_file.txt`라는 임시 파일이 생성됩니다. 만약 코드 실행 중에 예기치 않은 오류가 발생하여 파일이 닫히지 않는 경우에는 `File.close` 대신 `File.close!` 함수를 사용하여 강제로 파일을 닫을 수 있습니다.

## 깊이 파고들기

임시 파일을 생성하는 방법은 위 예시 코드와 같이 간단하지만, 실제로는 여러 가지 옵션을 설정할 수 있습니다. `File.open` 함수는 다양한 옵션을 지원하며, 자세한 내용은 [공식 문서](https://hexdocs.pm/elixir/File.html#open/2)에서 확인할 수 있습니다. 또한 `File.temp_dir!` 함수 대신 `File.temp_dir` 함수를 사용하면 원하는 디렉토리 경로를 지정할 수 있습니다.

## 더 알아보기

- [Elixir File 모듈 공식 문서](https://hexdocs.pm/elixir/File.html)
- [Elixir Base 모듈 공식 문서](https://hexdocs.pm/elixir/Base.html)
- [Elixir 임시 파일 생성 관련 질문과 답변](https://stackoverflow.com/questions/33296189/creating-a-temp-file-in-elixir)

## 관련 자료

- [Elixir: 임시 파일 만들기에 유용한 팁들](https://medium.com/expedia-group-tech/elixir-cool-tips-for-creating-temp-files-86c5c8c52806)
- [Elixir: 임시 파일 다루기](https://gist.github.com/christopheradams/240c7361c362e657e90b)