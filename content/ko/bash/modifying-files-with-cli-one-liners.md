---
title:                "CLI 한 줄 명령어로 파일 수정하기"
date:                  2024-01-26T22:20:58.627422-07:00
model:                 gpt-4-0125-preview
simple_title:         "CLI 한 줄 명령어로 파일 수정하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data and Text Processing"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/modifying-files-with-cli-one-liners.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

CLI(Command Line Interface)로 파일을 한 줄 명령어로 수정하는 것은 터미널에서 바로 파일을 빠르고 목표한 변경을 하기 위한 것입니다. 프로그래머들이 이 방법을 사용하는 이유는 빠르고, 스크립트화할 수 있으며, 리눅스와 같은 환경에서 작업할 때 실제 에디터를 열지 않고 수정을 적용하는 가장 직관적인 방법이 종종 됩니다. 이는 sed, awk, grep 및 기타 명령 줄 도구의 힘을 활용하여 파일 내용을 즉석에서 검색, 교체, 삽입, 또는 삭제합니다.

## 어떻게:

몇 가지 기본 예제를 살펴보겠습니다:

1. `sed`를 사용하여 파일 내 **텍스트 교체**:
   ```Bash
   sed -i 's/oldText/newText/g' filename.txt
   ```
   이 명령은 `filename.txt`에서 `oldText`를 찾아 `newText`로 교체합니다.

2. 파일에 **텍스트 추가**:
   ```Bash
   echo "New line of text" >> filename.txt
   ```
   `filename.txt`의 끝에 새로운 텍스트 줄을 추가합니다.

3. `sed`를 사용하여 특정 문자열을 포함하는 **줄 삭제**:
   ```Bash
   sed -i '/stringToDelete/d' filename.txt
   ```
   `filename.txt`에서 `stringToDelete`를 포함하는 줄을 삭제합니다.

4. `grep`을 사용하여 패턴과 일치하는 **줄 추출 및 출력**:
   ```Bash
   grep 'patternToMatch' filename.txt
   ```
   패턴과 일치하는 `filename.txt`의 줄을 표시합니다.

## 심층 분석

CLI로 파일을 한 줄 명령어로 수정하는 기술은 Unix 자체만큼 오래되었으며, `sed`, `awk`, `grep`, `cut`과 같은 도구에 크게 의존합니다. 이 유틸리티는 Unix 초기에 텍스트 처리 작업을 효율적으로 처리하기 위해 설계되었으며, 당시 혁신적이었던 파이프라인 개념을 활용하고 있습니다.

**대안들**: 이 한 줄 명령어들은 강력하지만, 더 복잡한 데이터 구조나 바이너리 파일을 다룰 때는 한계가 있습니다. 이러한 경우, 고급 파싱 및 데이터 조작 기능을 갖춘 Python 또는 Perl과 같은 고급 스크립팅 언어가 더 적절할 수 있습니다.

**구현 세부사항**: 이 도구들과 작업할 때는 패턴 매칭과 텍스트 조작의 기반인 정규 표현식(regex)을 이해하는 것이 중요합니다. 게다가, macOS와 Linux에서 각기 다르게 작동하며, 특히 macOS에서는 `-i` 옵션에 백업 확장자 인수를 포함해야 할 수도 있는 `sed`의 제자리 편집 `-i` 옵션은 모든 시스템에서 동일하게 작동하지 않습니다.

## 참고 문헌

- GNU `sed` 메뉴얼: [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html)
- The AWK Programming Language: [https://www.cs.princeton.edu/~bwk/btl.mirror/](https://www.cs.princeton.edu/~bwk/btl.mirror/)
- Grep 매뉴얼 페이지: [https://www.gnu.org/software/grep/manual/grep.html](https://www.gnu.org/software/grep/manual/grep.html)
- 정규 표현식 정보: [https://www.regular-expressions.info/](https://www.regular-expressions.info/)
