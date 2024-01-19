---
title:                "텍스트 검색 및 교체"
html_title:           "Elixir: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 뭐하고 왜?
텍스트 검색 및 바꾸기는 특정 문자열을 찾아 다른 문자열로 대체하는 것을 말합니다. 이 기능은 코드 수정, 데이터 변환, 자동화 작업 등에 프로그래머들이 자주 사용합니다.

## 어떻게 하는지:
간단한 예시로 어떻게 사용하는지 알아봅시다. 다음은 'hello'를 'hi'로 바꾸는 코드입니다:

```Bash
$ echo "Hello, World!" | sed 's/Hello/Hi/'
```
이 코드를 실행하면, 결과는 다음과 같습니다:

```Bash
Hi, World!
```
이 예시에서, "sed"는 Stream EDitor의 줄임말로 텍스트를 조작할 수 있는 유니버셜 툴입니다. 's/Hello/Hi/'는 'Hello'를 'Hi'로 대체하라는 명령입니다.

## 심화 학습
더 깊이 학습하기 위해 이 기능의 (1) 역사적 배경, (2) 대안, 그리고 (3) 세부 구현 방법에 대해 알아봅시다.

(1) sed는 70년대에 개발되었어요. 이후로 텍스트 편집 및 조작에 우리에게 무궁무진한 가능성을 제공했습니다.

(2) 검색 및 대체 작업에는 Perl, Python, AWK 같은 다른 언어 또는 도구도 사용할 수 있습니다. 

(3) sed의 's' 명령어는 검색 및 대체 작업에 사용됩니다. 's/foo/bar/' 형식으로 쓰면 'foo'를 'bar'로 대체하는 작업을 수행합니다.

## 참고 자료
더 많은 정보를 찾으려면 아래 링크를 확인하세요:
- GNU sed: https://www.gnu.org/software/sed/
- AWK: https://www.gnu.org/software/gawk/
- Perl: https://www.perl.org/
- Python: https://www.python.org/

이해가 필요한 부분이 있다면 언제든 질문해주세요! Happy coding~