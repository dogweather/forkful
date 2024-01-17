---
title:                "텍스트 검색 및 대체하기"
html_title:           "Fish Shell: 텍스트 검색 및 대체하기"
simple_title:         "텍스트 검색 및 대체하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
검색 및 대체 텍스트는 프로그래머가 특정 텍스트를 찾고 원하는 형식으로 바꾸는 작업입니다. 이 작업을 하는 이유는 주로 코드를 더 효율적이고 간결하게 만들기 위해서입니다.

## 하는 방법:
```Fish Shell``` 코드 블록 내에서 코딩 예제와 샘플 출력입니다. 

### 검색:
```fish
echo "Hello World" | sed 's/World/Universe/'
```
**출력:** Hello Universe

### 일괄 변경:
```fish
echo "Hello World" | sed 's/Hello/Hi/g'
```
**출력:** Hi World

## 깊이 들어가보기:
**역사적 맥락:** 텍스트 검색 및 대체는 프로그래밍에서 오래 전부터 사용되어 온 기법입니다. 이전에는 ```sed```와 같은 툴이 많이 사용되었지만 Fish Shell에서는 바로 사용할 수 있는 내장 명령어로 제공됩니다.

**대안:** Fish Shell에서는 다양한 방법으로 검색 및 대체 작업을 수행할 수 있습니다. 일반적인 ```sed``` 대신에 ```string replace``` 함수를 사용할 수도 있습니다.

**구현 방법:** Fish Shell에서 텍스트 검색 및 대체 작업은 내장 명령어로 구현되어 있으며, 정규식을 지원합니다. 일반적으로 지원되는 기능들을 사용하여 효율적으로 작업할 수 있습니다.

## 관련 자료:
- [Fish Shell 공식 홈페이지](https://fishshell.com/)
- [Fish Shell Stack Overflow 페이지](https://stackoverflow.com/questions/tagged/fish)
- [Fish Shell GitHub 페이지](https://github.com/fish-shell/fish-shell)