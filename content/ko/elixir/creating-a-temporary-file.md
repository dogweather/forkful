---
title:                "임시 파일 만들기"
html_title:           "Elixir: 임시 파일 만들기"
simple_title:         "임시 파일 만들기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 나와 뭐할까? 
임시 파일 생성이 무엇인지와 프로그래머들이 이를 하는 이유를 간단히 설명하는 두-세 문장.

임시 파일 생성은 데이터를 임시적으로 저장하기 위해 많이 사용되는 프로그래밍 기술 중 하나입니다. 이는 큰 데이터 집합을 처리하는 데 유용하며, 프로그램이 실행되는 동안 메모리를 차지하지 않으므로 더욱 효율적입니다.

## 어떻게 하나요?
Elixir 코드 블록(```Elixir ... ```) 내부에 코딩 예제 및 샘플 출력을 포함시킵니다.

```elixir
# 예제 1: 임시 파일 생성
{:ok, file} = File.Temp.mkdir()
file_path = File.join(file, "temp.txt")
File.write(file_path, "임시 파일 생성 예시")
```

```
# 출력: temp.txt 파일에 "임시 파일 생성 예시" 쓰기
:ok
```

## 깊게 파헤쳐보기
(1) 역사적 배경, (2) 대체 방법, (3) 임시 파일 생성 구현 세부 정보와 같은 추가 정보를 제공합니다.

1. 임시 파일 생성은 많은 언어와 운영 체제에서 지원되고 있으며, 일반적으로 파일 시스템을 사용하여 임시 저장소를 만듭니다. 이는 1960년대 코먼 랭구어에서 처음 사용되었습니다.

2. Elixir에서는 다른 방법으로도 임시 파일을 생성할 수 있습니다. 예를 들어, `File.open/2`를 사용하여 파일을 열고 그냥 버리는 것으로도 임시 파일을 생성할 수 있습니다.

3. `File.Temp` 모듈은 `:temp` 디렉토리 아래에 임시 디렉토리를 생성하고, `File.join/2` 함수를 사용하여 파일 경로를 만들고, `File.write/2` 함수를 사용하여 파일에 데이터를 쓰는 방식으로 임시 파일을 만듭니다.

## 또한 확인해보세요
관련 정보를 얻을 수 있는 링크를 제공합니다.

- [Elixir 공식 문서: File.Temp](https://hexdocs.pm/elixir/File.Temp.html)
- [Elixir 프로그래밍 언어 소개 글](https://ko.wikipedia.org/wiki/%EC%97%94%EB%A6%AC%EC%85%80%EB%9F%AC_(%ED%94%84%EB%A1%9C%EA%B7%B8%EB%9E%A8))