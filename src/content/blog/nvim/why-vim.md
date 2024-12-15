---
author: ccbhj
pubDatetime: 2024-10-11T20:19:03Z
modDatetime: 2024-10-12T23:15:07Z
title: "vim, editing with fun and efficiency"
slug: "vim_editing_with_fun_and_efficiency"
featured: true
draft: false
tags:
  - vim
description:
  Some jibber-jabber about vim.
---

## Table of contents

## Intro
When I was just a college student, I was introduced to Ubuntu, the most frequent talked Linux OS at that time around me, by one of my tutors(thanks to him, I am not poisoned by Windows or OS X). Of course I installed the Ubuntu in my laptop once I got home, but after I installed it I gotta change the distribution source because I couldn't stand the download speed when apt updating(for I live in China). However I couldn't save and exit the editor in terminal like everyone else who asks "How to save and exit vim" in reddit and stackoverflow. After some searching and digging in google, I was attracted by this amazing editor and decided to a few night to pick it up.

To me, vi/(neo)vim is a great technical product that really changes one's way of editing, it literally introduced a whole new way of editing and bring me a lot fun while editing. Six years later, I am still using vim(neovim actually) to edit anything like coding(you don't need pay for a IDE to code) or writing documents. And I still get excited once in while when learning a new keyboard shortcut or a plugin. So I decide to share some thoughts here, to show you why vim change the way of editing and bring me fun while coding.


## A whole new way of editing
I think what really keeps me using vim for six year is the way of switching navigation and inputting modes during editing. This whole new way of editing is the magic that let people develops a habit to use vim that you can never get rid of. You might have heard of the debate between vim and emacs, people using emacs just hate vim but guess what, the vi layer for emacs [evil](https://github.com/emacs-evil/evil) has 3.4k stars in github, got the irony here?

It's kind of painful for me to edit text without vim. I just can't keep pressing arrow keys for like a thousand time just to move my cursor to the next few words, but most people I work with do. It sometimes drives me crazy to see people move away from their keyboard to the mouse or hitting the arrow keys again and again just to move the cursor to the next word and I was like "DUDE, STOP HITTING YOUR GODDAMN RIGHT ARROW !!! JUST USE VIM !!!"

### NORMAL mode
The magic in vim is **modes**, which separates moving(NORMAL mode), editing(INSERT mode) and selecting(VISUAL mode). By default vim is in NORMAL mode, which allows you to:
1. Use more key to move around your cursor. Now that you won't input anything and mess up the text in NORMAL mode, you can use not only arrow keys but also 'hljk' to move left/right/down/up, 'e' to move to the end of the current word, 'w' to the start of the next word, '0' to the start of the line, '$' to the end of line and so on(I can do this all day). You don't need any modifier key, which is quite the common usage for assigning different functionality to the same key in most of the editors or IDEs, and it hurts your pinky finger!(Yes I was thinking about emacs here).
2. Moving around quickly. You can combine numbers with the moving keys I refer above, for example, you can use , '2j' to move two lines down, '3w' to move to the front of the next three word. What's more, vim can recognize text objects like '{}' or '()' in your code, with the powerful '[' or ']' as the first key, the second key like '(' will take you back to the '(' of the current parentheses pair, and '}' will take you to the '}' of the current block, and it can also combine with numbers for example:
```lisp
  (cons 1 (cons 2 (cons 3 nil))
; ^       ^       ^     |
; 3[(    2[(      [(    |
;                       |
;            initial cursor position here

```
3. View and scroll text with hands on your keyboard. When it comes to scrolling text, we usually don't have much choice but just using our mouse. But in vim's NORMAL mode, you can just use 'ctrl-e'/'ctrl-y' to scroll down or up one line(still, you can add numbers to the front of 'ctrl-e' to scroll down N lines), 'ctrl-d'/'ctrl-u' to scroll down or up half of the screen, and 'zz' scroll to the middle of the screen  precisely and quickly. You literally don't need no mouse in vim.

### INSERT mode
The name "INSERT" seems quite confusing but it actually indicates that this mode is more than just editing text but the action to enter the editing mode. Here are some most used keys that used to enter the INSERT mode:
| key | action |
| ------------- | -------------- |
| i(insert) | start editing **before** the cursor  |
| a(append) | start editing **after** the cursor  |
| I(Insert) | start editing **before** the current line  |
| A(Append) | start editing **after** the current line  |
| rx(replace) | replace letters under the cursor with character 'x'  |
| x(delete) | delete letters under the cursor |
| dk(delete) | delete letters between the current cursor and where key 'k' jumps(e.g 'dw' allows you to delete the current word) |
| ck(change) | delete letters between the current cursor and where key 'k' jumps and start editing(e.g 'cw' allows you to delete the current word and start editing) |

There are much more ways of entering INSERT mode but I think you've got the point here. Every editor allows you to input text, since that's what editor do but what vim allows you here is to **edit the text in the precise position or spare you the time from switching between to NORMAL mode**. Imagine that if we don't have these key, it would takes us 2 keys('0' to jump to the start of the current line and 'i' to start editing) to start editing from the start of the current line, but an 'I' will take you there directly, and if we don't have 'd', it will take you 3 keys to delete just one letter('a' to start editing after cursor, and one backspace to delete the letter and <ESC> to switching back to NORMAL mode). You can treat it as some compromise for the "mode" mechanism, but it does reduce the time between switching from NORMAL mode to INSERT mode and indeed improves the editing experience. 
Remember the text objects in NORMAL mode we talked about above? Well, we support that in INSERT mode too. By typing 'ci)', we can change text **inside** the parentheses pair, and 'da)' can delete all the text 'around' parentheses including parentheses. In case you haven't notice, 'd' and 'c' behavior are actually combine with those keys for moving cursor, which means you can delete 3 words just by typing 'd3w' and change three lines by typing 'c3j'. That's what I am talking about when I said **vim change the way of editing into programming - when we are editing in vim, we are actually giving commands to it by typing keys**.

### VISUAL mode
In short, VISUAL mode is for selecting section of text by moving cursor around. A 'v' could get you into letter selecting mode, and 'V' is for line selecting mode. And then you can use keys in NORMAL to moving the cursor around to select text. After that, you can "delete" them by typing 'd', replace them by typing 'r' and a letter for replacing text, 'c' for changing the whole text. With VISUAL mode, you don't have to use mouse just to select text either limited by 'w' that can only delete a word, '0' that can only delete text till the start of the line or '$' till the end of the line. Vim also provides the BLOCK VISUAL mode which allows you to select text across lines and edit each lines in the block. After selecting a text block, you can use type 'A' to append text, after pressing 'ESC' the text will appear at the end of each line in the whole block:
![VISUAL EDITING](https://vhs.charm.sh/vhs-2lz2nelAlvN4zJLvVNO9uR.gif)

As I also show in the GIF above, this is really helpful when it comes to comment code or draw table. This is another advantage that mode mechanism in vim brings us, because multi-line editing is nothing more than the combination of VISUAL mode and INSERT mode, it doesn't just import another mode nor assign a special key to it(just like the way vscode did with 'alt').

### Modes
As Alan Kay said in his presentation [Power Of Simplicity](https://tinlizzie.org/IA/index.php/Alan_Kay,_2015:_Power_of_Simplicity) **"You get simplicity by finding a slightly more sophisticated building block"**, and modes in vim is the building block of editing. Keys in different mode can be assigned to different functionality without any modifier like 'ctrl' and 'alt'. There are a lot of downside to use 'ctrl' or 'alt' to assign one than one functionality to keys:
1. Modifier might be different in different platform, for example, alt in Windows/Linux vs option in OS X.
2. It causes confliction between different software. That's why I can't accept [VsVim](https://marketplace.visualstudio.com/items?itemName=JaredParMSFT.VsVim) in vscode nor [ideavim](https://github.com/JetBrains/ideavim) in JetBrains IDEs, they usually comes with key confliction and you have to change your key map to adapt them.
3. It tires out your pinky finger to reach ctrl in your keyboard. I always have to map caplocks to left ctrl which eases the pain to my pinky when using too much ctrl.

## Beyond editing
Years of using vim I am still digging and learning something new in vim and getting goosebumps for it. Here are some tricks I found most interesting in vim. 

### Registers
Imagining you have opened a few files in an editor and you want to copy a few words from one file to a different place in the other file, what would you do? People not using vim will use mouse to select one word to copy in file 1, go to the file 2, paste the word and then go back to the file 1 for the next word. It doesn't sound that bad until you repeat it for like 10 times. But in vim, we can use **registers**.
Registers stores text in somewhere that is accessible using keys. By using '"' and a label key to refer a register, and then you can use it to store text or extract text stored in them. For example, '"ayw' allows you to choose register "a, copy a word and store it into the register, later you can use '"ap' to paste it to other place. If you don't want to choose a specified register every time you copy something, a default register called '""' will be used(Don't got confused here, "" is a unnamed register instead of empty string).
Besides named register ranging from "a to "z and the unnamed register "", we have 10 types of registers in vim:
1. The unnamed register "";
2. 10 numbered registers "0 to "9. These ten registers are used as a ring buffer, "0 store the most recent yanked text, "1 store the most recent deleted text that is large than one line(or the small delete register will be used). And every time the "1 is filled, the old content will be shifted to "2, "2 goes into "3 and so forth;
3. The small delete register "- used to store most recent deleted text that is less than one line;
4. 26 named registers "a to "z or "A to "Z;
5. Three read-only registers ":, "., "%. These three cannot be modified and can only be used for pasting text, ": contains the most recent executed command, ". for most inserted text, "% for the current file name.
6. Alternate buffer register "#
7. The expression register "= allows to evaluate a math expression you use can use the result by 'p'.
8. The selection registers "* and "+. "* is synchronized with selection clipboard using GUI, while "+ synchronized with system clipboard so that you can read text from system clipboard.
9. The black hole register "_ is like the device "/dev/null" in Unix, everything goes into it will disappear, you can use it to delete text without affecting the normal registers.
10. Last search pattern register "/ store most recent searched pattern.

These 10 types of register provide access to content synced from various way - user-defined buffer, system clipboard, yank/delete/change action, which I think it's significant and powerful, however most of editors don't have it.

### Macros
Macros allows you record a sequence of action, store it into a register(using `q` and a register name) and you can reply it again(using `@` and a register name) in any other place. Imagining you have want to append comma to the end of each line, what would you do? Using block visual mode? No it won't work since each line might have different length, or choose the normal way to add it line by line and repeat the same thing for one thousand times? No, by storing the macro in register 'q' using 'qq', we can record the operation sequence 'A,' to append comma to the end of the current line, then select all the lines we need and use command "norm"(execute key sequence in NORMAL mode) to reply the macro in register q to each line like this:
![multi-line macro](https://vhs.charm.sh/vhs-2gpJPQbpmVqlgBipycgGRp.gif)
But what role does register play here? Well, when I say "store a sequence of action in a register", it means vim basically save a sequence of key to be typed when editing. For the example we used above, the action of sequence is `A,`, so we can literally save a sequence of keys to be typed into another register, and replay it to another line:
![save macro into register](https://vhs.charm.sh/vhs-4GCweNbmtiB5pHh36vVHDH.gif)
Not sure if you feel the magic here but I was really astonished when I first saw this because it is just brilliant. Guess what, the GIF I produced in this blog is actually generated by inputting key sequence using [vhs](https://github.com/charmbracelet/vhs). All I have to do is input some sort of command, and it could just turn it into GIF:
```
# /tmp/vim.tape
Set TypingSpeed 200ms

Type "vim /tmp/test.txt"   # input command to start vim
Enter
Sleep 500ms
Escape 2

Type "ggCA,"               # go to the first line, change the whole line and input "A,"
Escape
Backspace

Ctrl+V                     # enter VISUAL BLOCK mode
Type "l"                   # move the cursor left
Sleep 500ms 

Type '"wy'                 # copy the selected text into register 'w'
Sleep 500ms 

Type "j@w"                 # move down the cursor and apply the key sequence in register 'w' to the current line
Sleep 500ms
Escape

Type "j0@w"                # move down the cursor, go to the start of a line and apply the key sequence in register 'w' to the current line
Sleep 500ms
Escape

Type ":q!"                 # enter command to quit vim
Enter
Sleep 500ms
```
With one line of command `vhs /tmp/vim.tape` you can get a GIF I demonstrated above, what a great tool! But to be honest, you just can't perform this with vscode, sublime or any JetBrains IDEs because you can't neither start them in bash shell nor have the confidence to just feed a key sequence to it and expect it works! But vim can, vim works just like a machine, accept keys as commands and give you the result as you expect! **That's what I love about vim, so simple, precise and predictable.**

### DRY(Don't Repeat Yourself)
As coders, we may have heard of DRY principle. You can see it in vim too. To avoid repeating yourself, vim provides you some shortcuts to 
- repeat the recent action by pressing `.`:
![repeat action](https://vhs.charm.sh/vhs-6uywSNHaf64qO9OYQx8RBv.gif)
- go back to the place before last jump by pressing '``':
![going back before last jump](https://vhs.charm.sh/vhs-6fjW5Hosy9FGgydfNd8pPp.gif)
- repeat the recent macro by typing `@@`:
![repeat macro 'A,'](https://vhs.charm.sh/vhs-4vm4gtixed4WeIxmE9ugky.gif)
- repeat the last search by typing `//`:
![repeat last search of "foo"](https://vhs.charm.sh/vhs-Pduis3ZQgjhsYwXXTzAZ7.gif)

## Ecosystem
The plugin ecosystem in vim is what bring more fun to this editor. Vim embeds a script language vimscript for configuration and developing plugin while neovim, a fork of vim introduced lua. Since two years ago I had switched to neovim and it has been a great journey because it really blooms the whole vim community with lua used configuration and plugin development, asynchronization engine and great terminal/float window support. In the last 30 years, vim/nvim has formed a large community and a lot of devoted developers comes out with a bunch of interesting and productive plugin.

Here I gonna introduce you some plugins that I find most productive, most of them are written in lua since I had switched to neovim for quite a long time.
1. [folke/lazy.nvim](https://github.com/folke/lazy.nvim) - Plugin manager

lazy.nvim nowadays is kind of a must-have plugin for nvim. Not only does it provides you the basic plugin management like install, uninstall and upgrade, but it also help you to reduce the startup time by lazily loading your plugins. You can easily define when to load a plugin, maybe for a certain file type or when a certain command got executed or some events like inserting text or entering a buffer.
![Preview](https://user-images.githubusercontent.com/292349/208301737-68fb279c-ba70-43ef-a369-8c3e8367d6b1.png)

2. [neovim/nvim-lspconfig](https://github.com/neovim/nvim-lspconfig) - Language Support

One good thing that Microsoft had brought to the world is language server protocol. Not just vim, editors like emacs, sublime text are all benefit from it because it allows language developers to support syntax highlighting, completion, diagnosis and symbol definition without concerning what the editor users are using. But even though we've got language servers for you, editor still need to support the protocol. That's what this repo do, to be a guideline to setup language server.
The support of language server protocol is the reason why you don't need a IDE. **You don't need to pay $779(the exact price of JetBrains All Products Pack) for one year to write code.** With neovim, you can easily install and setup more than 300 language server in a few minutes. 


3. [nvim-telescope/telescope.nvim](https://github.com/nvim-telescope/telescope.nvim) - Fuzzy Finder

telescope.nvim is another must-have plugin in nvim. It provides you a fuzzy finder and previewer for basically anything in vim like files, git integration, grep string, register, buffer. Besides that, you can write your own extension and integrate with telescope easily. [telescope-file-browser.nvim](https://github.com/nvim-telescope/telescope-file-browser.nvim) is the one I love most, it provides you a file browser in telescope and you can navigate to any directory and preview the content in any file.
![Preview](https://i.imgur.com/TTTja6t.gif)

4. [nvim-treesitter/nvim-treesitter](https://github.com/nvim-treesitter/nvim-treesitter) - Language Parser Tools

Tree-sittter provides a simple and easy way to use the interface for [tree-sitter](https://github.com/tree-sitter/tree-sitter) in neovim. In a word, it provides you easy accessibility to language text objects for: 
- syntax highlighting, you can use it to write your own color scheme
- [text objects](https://github.com/nvim-treesitter/nvim-treesitter-textobjects), remember the text object I talked about above. Vim only have text block object inside '()' or '{}', but with tree-sitter, you can have more text object like function, class, struct and so on, so that when you type `daf` you can delete(`d`) text around(`d`) the current function(`f`), or type `cic` to change(`c`) text inside(`i`) a class object(`'c`).
- [context](nvim-treesitter/nvim-treesitter-context), sticky context header indicating what function/class the cursor in without see the declaration in the current screen.
![Preview](https://github.com/nvim-treesitter/nvim-treesitter-context/raw/master/static/demo.gif)

5. [folke/flash.nvim](https://github.com/folke/flash.nvim) - enhanced character motions

Another plugin by almighty [folke](https://github.com/folke) that allows you to jump literally anyway with two keys. When you press `f` and a letter you want to search, flash show you a couple label ranging for a-z or A-Z, and you can press key of the label you want to jump then flash will take you there. This mechanism apply to the whole screen, even all the buffers so it could literally jump to anywhere you like with two key. What's more, it integrates with tree-sitter, and allows you the jump to the current or outter language text object like function, struct, class.

6. [tpope/vim-surround](https://github.com/tpope/vim-surround) - easy pair manipulation

[tpope](https://github.com/tpope/) is another great vim plugin developer. This plugin allows you to change, delete, insert pairs of '()', '{}', '""' and even XML/HTML tag, which really improves the editing experience.
For example, by pressing `ysw"`(make this word surrounded with '"') to `hello` you can add a pair of '"' around this word and it will become `"hello"`. Then by pressing 'ds"'(delete the " pair surrounded) you can delete the " pair around this word.

## You don't need no IDE
People I works with usually don't use vim/nvim, 99.9% of them use IDEs from JetBrains. They often seem astonishing when seeing me use vim to code and ask "Why don't you use IDE?" or "How do you type so fast?". To be honest I took it as compliment but I think the really question should be "Why do you need IDE?" and "Why do you type so slow?".

Paying 799$ a year to JetBrains to write code sounds insane to me. People used to develop great software like Linux, GCC, or programming languages with just plain text editors like vim or emacs. But people nowadays can't even write code without IDEs. This is what I called **[Vendor Lock-in](https://en.wikipedia.org/wiki/Vendor_lock-in)**. In Joe Armstrong(one the inventors of erlang)'s presentation [A Guide for the Perplexed](https://www.youtube.com/watch?v=rmueBVrLKcY), Vendor Lock-in was listed in 4 really bad things for this situation when everyone can't code without IDE will only do good to the big companies like JetBrains. Imagining one day JetBrains suddenly decides not to provide any IDE to you, and all of a sudden you can't write a single line of code because you forget how to write code without IDE. What a tragedy would that be! So pick up any open source editor right now and step our of your comfort zone by not using any IDE. Don't be lazy and just learn yourself some bash shell, gcc, make, vim/emacs and git!

## The author of vim Bram Moolenaar
Last year(3 August 2023), the author of vim, Bram Moolenaar, has passed away. This man devoted most of his lifetime to vim and charity. He has made vim open-source and charityware(a software licensed in a way that benefits a charity), and encourage people to donate [ICCF_Holland](https://en.wikipedia.org/wiki/ICCF_Holland). Until today, we can still the banner "Help poor children in Uganda!" once we open vim. Bram Moolenaar, what a dedicated, and selfless human-being.
![Bram Moolenaar(source: wikipedia)](https://j11g.com/wp-content/uploads/2023/08/Bram_Moolenaar.jpg)

## Don't miss the fun while makes your life better
 Vim was first released at 2 November 1991 which was 33 years ago, how many software have you heard of that lives for 33 years and still be the best choice by millions of people? Although neovim, the fork of it, has recently became the trend but I think it still doesn't dim the glory of vim, for it show us a whole new way of editing and what we could've done with just an text editor.

I think in the post I have introduced you some magic in vim that you don't see elsewhere, and you gotta know that there's more of it for you to find out there. Some people might tell you that it is a total waste of time to learn vim or config vim. Well, people want to code will always find time to code regardless how much time spent on learning vim and we learn vim, config vim or even developing plugin for it. **We do it just for fun**(yes, you might heard of it before by Linux Torvalds). 

Besides vim could really make your life better. I really love the quote **"Usability is high-leverage"** from the presentation [We can have nice things](https://youtu.be/Bt-vmPC_-Hoby) by Justin M. Keyes. He stated that **when a small problem got fixed or improved, the benefits accumulate over time**. That's what learning vim does! Most of people just don't get it that, though we don't see much benefit after learning one or two shortcut or integrating one or two plugins, it will definitely save you a lot of time every time you want to edit something. It won't save you like 10 minutes or 100 hours at once but it will help you develop a habit of editing with efficiency.

Hope you enjoy editing!
