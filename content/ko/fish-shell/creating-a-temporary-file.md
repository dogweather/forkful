---
title:                "임시 파일 생성하기"
html_title:           "Python: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

임시 파일(temporary file)은 특정 작업을 위해 일시적으로 생성되는 파일입니다. 프로그래머는 데이터를 임시적으로 저장하거나, 복잡한 작업을 단계적으로 처리하기 위해 임시 파일을 사용합니다.

## 실행 방법:

Fish Shell에서 임시 파일을 생성하는 방법을 살펴봅시다.

```Fish Shell
# tempfile 함수를 사용하여 임시 파일 생성하고, 경로를 변수에 저장
set temp (mktemp /tmp/tempfile.XXXXXX)

# 임시 파일에 내용을 작성
echo "Hello, Fish Shell!" > $temp

# 임시 파일의 내용 출력
cat $temp
```

위 코드를 실행하면, 아래와 같은 출력을 확인할 수 있습니다.

```Fish Shell
Hello, Fish Shell!
```

## 깊이 있는 내용:

1. **역사적 맥락**: 임시 파일은 컴퓨터가 발명된 이래로 프로그램간 데이터를 임시적으로 저장하고 공유하며 효율적인 작업을 위해 사용되고 있습니다.

2. **대체 방안**: 임시 파일 스트림(tempfile)을 사용하여 메모리에 데이터를 임시로 저장하는 방법도 입니다. 이 방법은 디스크 I/O를 건너뛸 수 있으므로 빠르지만, 사용 가능한 메모리 양에 제한이 있습니다.

3. **구현 세부 내용**: "mktemp"는 임시 파일을 생성하며 고유한 경로를 반환합니다. "/tmp" 디렉토리는 일반적으로 임시 파일이 저장되는 곳이며, "XXXXXX"는 임의의 문자열이 됩니다.

## 참고 사항:

- Fish Shell 공식 문서: [링크](http://fishshell.com/docs/current/)
- 임시 파일과 관련된 Unix 프로그래밍 도구: [링크](http://www.gnu.org/software/libc/manual/html_node/Temporary-Files.html)
- 효율적인 프로그래밍을 위한 임시 파일 사용법: [링크](https://www.tldp.org/LDP/abs/html/tempfiles.html)