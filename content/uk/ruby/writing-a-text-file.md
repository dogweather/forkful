---
title:                "Створення текстового файлу"
date:                  2024-01-19
html_title:           "Arduino: Створення текстового файлу"
simple_title:         "Створення текстового файлу"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? / Що і чому?
Writing a text file in Ruby means saving data to a file on your disk. Programmers do this to store info, like logs or user data.

## How to: / Як це зробити:
```Ruby
# Open and write to a file
File.open('example.txt', 'w') do |file|
  file.puts "Привіт, світ!"
end

# Append to existing file
File.open('example.txt', 'a') do |file|
  file.puts "Ще один рядок."
end
```

Sample output в 'example.txt':
```
Привіт, світ!
Ще один рядок.
```

## Deep Dive / Поглиблений аналіз:
Writing to text files has been basic to programming since early days. Alternatives include databases or cloud storage. In Ruby, `File.open` with a block auto-closes the file. Beware of file permissions and encodings.

## See Also / Дивись також:
- Ruby API for File: https://ruby-doc.org/core/File.html
- Ruby IO class: https://ruby-doc.org/core/IO.html
- Guide to File I/O: https://www.tutorialspoint.com/ruby/ruby_input_output.htm
