---
title:                "Bash: 텍스트 파일 읽기"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

# 왜

당신이 텍스트 파일을 읽는 것에 대해 관심을 가지고 있을까요? 텍스트 파일을 읽는 것은 프로그래밍에서 매우 중요합니다. 이 기술을 배우면, 당신은 더 많은 정보를 분석하고 가공할 수 있습니다. 게다가, 텍스트 파일을 읽는 것은 쉽고 빠르며 유용합니다.

# 방법

텍스트 파일을 읽는 가장 기본적인 방법은 `read` 명령어를 사용하는 것입니다. 예를 들면, 다음과 같은 코드를 사용할 수 있습니다.

```Bash
file="example.txt"
while read line; do
  echo "$line"
done < "$file"
```

위의 코드는 텍스트 파일의 모든 줄을 한 줄씩 읽어서 화면에 출력합니다. 여기서 `example.txt`는 당신이 읽고 싶은 파일의 이름입니다. 또 다른 방법으로는 `cat` 이용하여 텍스트 파일의 전체 내용을 확인하는 것입니다. 예를 들면, 다음과 같이 사용할 수 있습니다.

```Bash 
cat example.txt
```

이 명령어는 그저 텍스트 파일의 내용을 확인하는 것이지만, 당신은 여기서 추가적인 가공을 수행할 수도 있습니다. 예를 들면, `grep` 명령어를 사용하여 특정 패턴이나 키워드를 검색할 수 있습니다. 이외에도 `awk` 또는 `sed`를 사용하여 특정 줄 또는 문자열만 추출할 수 있습니다.

# 딥 다이브

텍스트 파일을 읽는 것은 단순한 작업처럼 보일 수 있지만, 실제로는 많은 작업과 원리가 있습니다. 텍스트 파일을 읽을 때, 우리는 파일을 조각조각 나누어 읽는 것입니다. 이러한 조각을 버퍼(Buffer)라고 부릅니다. 파일의 크기가 클 경우, 우리는 버퍼를 여러 번 사용하여 파일을 읽어야 할 수도 있습니다. 또한, 특정 인코딩 방식을 사용하는 파일을 읽을 때 문제가 생기기도 합니다. 이러한 문제들을 해결하기 위해서는 파일 관련 함수들에 대한 깊은 이해가 필요합니다.

# 관련 정보

- [Bash 공식 문서: 파일 읽기](https://www.gnu.org/software/bash/manual/html_node/Redirections.html)
- [Bash Academy: 파일과 스트림 사용하기](https://guide.bash.academy/input-output.html)
- [셸 프로그래밍에 대한 초보자 가이드](https://www.shellscript.sh/)