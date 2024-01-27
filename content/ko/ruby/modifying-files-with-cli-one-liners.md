---
title:                "CLI 한 줄 명령어로 파일 수정하기"
date:                  2024-01-26T22:25:28.884099-07:00
model:                 gpt-4-0125-preview
simple_title:         "CLI 한 줄 명령어로 파일 수정하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data and Text Processing"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/modifying-files-with-cli-one-liners.md"
---

{{< edit_this_page >}}

## 무엇과 왜?
루비에서 CLI(명령 줄 인터페이스) 한 줄 명령어를 사용하여 파일을 수정하는 것은 루비의 명령줄 옵션을 사용하여 터미널에서 직접 빠르고 종종 간단한 텍스트 조작을 수행하는 것을 포함합니다. 이 기술은 파일에 대한 일괄 변경, 콘텐츠 필터링, 또는 편집기를 열지 않고 편집 작업을 자동화할 필요가 있을 때 매우 소중합니다. 이는 루비의 텍스트 처리 능력을 스크립트로 편집할 수 있게 효율적으로 활용하는 것에 관한 것입니다.

## 방법:
여러 줄의 텍스트가 담긴 `example.txt` 파일이 있고 줄 순서를 반전하고 싶다고 가정해 봅시다. 루비를 사용하여 이를 한 줄 명령어로 수행할 수 있습니다:

```ruby
ruby -e 'puts File.readlines("example.txt").reverse'
```

또는, `data.txt`에서 "foo"의 모든 발생을 "bar"로 교체하려면 다음을 수행할 수 있습니다:

```ruby
ruby -i.bak -pe 'gsub(/foo/, "bar")' data.txt
```

이 명령어는 원본 파일의 백업(`data.txt.bak`)도 생성하여 루비가 데이터 안전을 고려하고 있음을 보여줍니다. 이 명령어들은 파일 내용을 변경하기 때문에 샘플 출력은 직접적으로 보이지 않지만, `cat data.txt`를 사용하여 변경 사항을 볼 수 있습니다.

## 심층 탐구
`-e` 플래그는 주어진 스크립트를 실행하도록 루비에게 지시하고, `-i`는 선택적 확장자로 백업 파일을 생성하는 것과 함께 제자리 편집을 가능하게 합니다. `-p` 플래그는 Unix/Linux의 sed처럼 입력을 통해 반복하며 스크립트가 적용된 후 각 줄을 출력합니다.

역사적으로 제자리 편집 및 명령줄 처리는 sed, awk, 그리고 perl이 지배했던 영역이었습니다. 그러나, 루비는 이러한 기능을 잘 통합하여 풍부한 문법과 내장 라이브러리로 인해 더 복잡한 조작을 가능하게 합니다.

파일 수정을 위한 대안으로는 더 간단한 작업에는 sed와 awk가 있고, 더 복잡한 처리에는 전체 루비 스크립트를 사용할 수 있습니다. 매우 큰 파일이나 복잡한 작업에 대해 루비 한 줄 명령어를 사용하는 것의 단점은 텍스트 처리를 위해 특별히 설계된 도구가 더 빠르게 실행될 수 있다는 것에서 성능 문제가 될 수 있습니다.

구현 측면에서, 루비가 파일을 제자리에서 처리할 때, 효과적으로 파일을 읽는 동안 임시 출력을 생성한 다음 이 출력으로 원본 파일을 대체합니다. 이 세부 사항은 데이터 손실을 피하기 위해 `-i` 플래그 사용시 백업 옵션 또는 신중한 테스팅의 중요성을 강조합니다.

## 참고 자료
- 루비의 명령줄 옵션에 대한 공식 문서: [https://www.ruby-lang.org/en/documentation/quickstart/3/](https://www.ruby-lang.org/en/documentation/quickstart/3/)
- 루비 대 sed와 awk의 텍스트 처리 비교에 대한 광범위한 비교: [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html)
- 파일 및 IO 처리에 대한 루비의 다루기에 대한 더 깊은 탐구: [https://ruby-doc.org/core-2.7.0/IO.html](https://ruby-doc.org/core-2.7.0/IO.html)
