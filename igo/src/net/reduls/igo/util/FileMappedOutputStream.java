package net.reduls.igo.util;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.MappedByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.FileChannel;

/**
 * ファイルにマッピングされた出力ストリーム
 * net.reduls.igo以下のパッケージではファイルにバイナリデータを出力する場合、必ずこのクラスを使うようになっている
 */
public final class FileMappedOutputStream {
    private final MappedByteBuffer mbb;

    /**
     * サイズを指定して出力ストリームを作成する
     *
     * @params filepath マッピングされるファイルのパス
     * @params size ファイル(=出力するデータ)のサイズ。このサイズを超えて出力が行われた場合の動作は未定義。
     */
    public FileMappedOutputStream(String filepath, int size) throws IOException {
	new File(filepath).delete();
	
	final FileChannel cnl = new RandomAccessFile(filepath,"rw").getChannel();
	try {
	    mbb = cnl.map(FileChannel.MapMode.READ_WRITE, 0, size);
	    mbb.order(ByteOrder.nativeOrder());
	} finally {
	    cnl.close();
	}
    }

    public void putInt(int value)     throws IOException { mbb.putInt(value); }
    public void putChar(char value)   throws IOException { mbb.putChar(value); }
    public void putShort(short value) throws IOException { mbb.putShort(value); }
    public void putString(String src) throws IOException {
	mbb.asCharBuffer().put(src);
	mbb.position(mbb.position()+src.length()*2);
    }

    public void close() {
	mbb.force();
    }
}