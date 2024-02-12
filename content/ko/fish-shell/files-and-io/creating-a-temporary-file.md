---
title:                "임시 파일 생성하기"
aliases:
- ko/fish-shell/creating-a-temporary-file.md
date:                  2024-01-20T17:40:13.226799-07:00
model:                 gpt-4-1106-preview
simple_title:         "임시 파일 생성하기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
임시 파일 생성은 데이터를 일시적으로 저장하기 위한 파일을 만드는 과정입니다. 프로그래머들은 처리 중인 데이터를 임시로 보관하거나, 동시성 문제를 피하기 위해 이를 사용합니다.

## How to: (사용 방법:)
Fish Shell에서 임시 파일을 만들고 사용하는 방법입니다. 직관적이고 간단하죠.

```Fish Shell
# 임시 파일 생성
set tempfile (mktemp)

# 임시 파일에 데이터 쓰기
echo "임시 데이터" > $tempfile

# 임시 파일 내용 확인
cat $tempfile

# 임시 파일 삭제
rm $tempfile
```

위 코드 실행 결과, 임시 파일에 '임시 데이터'가 저장되었음을 확인할 수 있고, 마지막에 파일을 삭제합니다.

## Deep Dive (심층 분석)
UNIX 시스템에서는 전통적으로 `/tmp` 폴더가 임시 파일 저장을 위해 사용되어 왔습니다. `mktemp` 명령은 이런 관습을 따르며 안전한 임시 파일 이름을 생성합니다. Fish Shell은 이러한 유닉스 명령들과 잘 통합되어 사용하기 쉽습니다. `mktemp`의 대안으로는 직접 파일 경로를 지정하여 touch 명령을 사용할 수 있지만, 이는 충돌 가능성을 열어두므로 추천되지 않습니다. 안전하게 임시 파일을 생성하여 사용할 때는 `mktemp`를 활용하면 됩니다.

## See Also (참고 자료)
- [Fish Documentation](https://fishshell.com/docs/current/index.html)
- [mktemp man page](https://man7.org/linux/man-pages/man1/mktemp.1.html)
- [UNIX /tmp directory](https://en.wikipedia.org/wiki/Temporary_folder)
