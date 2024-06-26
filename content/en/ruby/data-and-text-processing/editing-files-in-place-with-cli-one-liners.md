---
date: 2024-01-27 16:14:22.954897-07:00
description: "How to: Ruby provides a straightforward way to edit files in-place right\
  \ from the command line. Using Ruby's `-i` switch, you can tell Ruby to operate\u2026"
lastmod: '2024-03-13T22:45:00.546898-06:00'
model: gpt-4-0125-preview
summary: Ruby provides a straightforward way to edit files in-place right from the
  command line.
title: Editing files in-place with CLI one-liners
weight: 32
---

## How to:
Ruby provides a straightforward way to edit files in-place right from the command line. Using Ruby's `-i` switch, you can tell Ruby to operate directly on the provided file(s). Let's play with a few examples to see how this works in real life. Imagine you have a file `greetings.txt` with the following content:

```
Hello, world!
Hello, Ruby!
Hello, programming!
```

And you want to replace the word "Hello" with "Hi". Here's how you can do it:

```Ruby
ruby -i -pe "gsub(/Hello/, 'Hi')" greetings.txt
```

After running this command, `greetings.txt` will be updated to:

```
Hi, world!
Hi, Ruby!
Hi, programming!
```

If you're worried about potentially messing up data, Ruby has got you covered. By providing an extension to the `-i` switch, Ruby creates a backup before executing the changes. For instance:

```Ruby
ruby -i.bak -pe "gsub(/Hello/, 'Bye')" greetings.txt
```

Now, along with your edited `greetings.txt`, you'll find a `greetings.txt.bak` in the same directory, holding the original content.

## Deep Dive
The magic of in-place file editing in Ruby stems from its combination of Perl-like text processing capabilities and Ruby's own syntactic elegance. Historically, Perl was the go-to language for quick one-liner scripting, especially for text manipulation. Ruby adopted this paradigm, allowing for powerful command line scripting capabilities.

Alternatives for in-place editing exist in other languages, such as Perl itself and sed, a stream editor in Unix systems. Each has its strengths—Perl is known for its text processing prowess while sed is unmatched in its simplicity for stream editing tasks. However, Ruby offers a balance, providing robust text manipulation with a more readable and user-friendly syntax, especially for those already familiar with Ruby.

On the implementation front, Ruby's in-place editing works by renaming the original file, creating a new one with the original filename, and then writing the changes to this new file as it reads from the renamed original. This approach ensures the atomicity of the operation; either the entire file gets processed successfully, or no changes are made, protecting the integrity of your data during the editing process. This mechanism, combined with Ruby's exception handling, also provides resilience against interruptions, such as power failures or process kills, ensuring that at least the backup remains intact.

In summary, Ruby's in-place file editing is a testament to its utility as a scripting language, offering a blend of power, simplicity, and elegance for text manipulation tasks directly from the command line.
