---
title:    "Fish Shell: 임시 파일 만들기."
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 왜
---
파일을 임시로 생성하는 것이 중요한 이유는 다양합니다. 가끔은 프로그램이나 스크립트에서 임시 파일이 필요할 수 있기 때문입니다. 이 임시 파일은 저장해야 할 데이터가 많을 때, 롤백 시스템을 구현할 때 또는 간단한 텍스트를 저장해야 할 때 사용할 수 있습니다. 이러한 이유로 인해, 임시 파일 생성은 매우 유용한 기술입니다.

# 어떻게
---
```Fish Shell
# 임시 파일 생성
set temporary_file (mktemp)
echo "임시 파일 생성 완료: $temporary_file"

# 파일에 데이터 저장
echo "안녕하세요. 이것은 임시 파일에 저장된 데이터입니다." > $temporary_file

# 파일 읽기
cat $temporary_file
```

위 예제에서는 `mktemp` 명령어를 사용하여 임시 파일을 만들었습니다. 임시 파일 이름은 `$temporary_file` 변수에 저장되며, 마지막으로 파일에 데이터를 저장하고 읽어옵니다.

# 딥 다이브
---
임시 파일을 생성하는 또 다른 방법으로는 `mktemp` 대신 `open` 명령어를 사용하는 것입니다. 이 명령어는 동일한 기능을 수행하지만 파일을 열어서 작업할 수도 있습니다.

또한, 임시 파일을 생성할 때 파일 이름으로 사용하는 `mktemp`의 기본 접두사가 `tmp.`이며, 이는 사용자가 원하는 대로 변경할 수 있습니다. 이를 통해 더 직관적인 파일 이름을 지정할 수 있습니다.

# 보기
---
### 관련 링크
- [Fish Shell 공식 문서: 임시 파일 생성](https://fishshell.com/docs/current/commands.html#mktemp)
- [Fish Shell 공식 문서: open 명령어](https://fishshell.com/docs/current/commands.html#open)
- [Temp 파일 (tmp/) Wiki](https://en.wikipedia.org/wiki/Temporary_file)