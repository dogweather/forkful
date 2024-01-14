---
title:    "Fish Shell: 문자열의 길이 찾기"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 왜
문자열의 길이를 찾는 것에 참여하는 이유를 간단히 설명합니다.

## 어떻게
```Fish Shell``` 코드 블록 안에 코딩 예제와 샘플 출력을 포함하여 설명합니다.

### 예제 1: 문자열의 길이 찾기 
```Fish Shell
set string "안녕하세요!"
echo $string | wc -m
```
### 샘플 출력: 7

### 예제 2: 공백을 포함한 문자열 길이 찾기 
```Fish Shell
set string "Hello, nice to meet you!"
set string (printf %q $string)
echo $string | wc -c
```
### 샘플 출력: 24

### 예제 3: 긴 파일 이름의 길이 찾기
``` Fish Shell
set filename "very_long_file_name.txt"
echo $filename | wc -c
```
### 샘플 출력: 23

## 깊이 파고들기
```Fish Shell```의 ```wc``` 명령어로 문자열 길이를 찾는 방법을 더 자세히 설명합니다. ```wc -c``` 옵션은 공백을 포함한 문자열의 길이를 찾아주며, ```-m``` 옵션은 한글 텍스트를 정확하게 처리할 수 있도록 해줍니다. 그리고 ```printf %q```를 사용하면 파일 이름이나 폴더 이름처럼 공백이 있는 문자열을 올바르게 인식하여 문자열의 길이를 찾을 수 있습니다.

## 관련 자료
[Fish Shell 공식 사이트](https://fishshell.com/)\
[Fish Shell 문서](https://fishshell.com/docs/current/)\
[Fish Shell 튜토리얼](https://fishshell.com/docs/3.3/tutorial.html)\
[유용한 Fish Shell 스니펫 모음](https://github.com/jorgebucaran/fisher)\
[생활코딩: 기초 셸 스크립트 과정](https://opentutorials.org/course/1058)\
[코딩도장: 셸 스크립트](https://dojang.io/course/view.php?id=5)