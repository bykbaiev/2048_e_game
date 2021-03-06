const path = require('path');

module.exports = {
    module: {
        rules: [
            {
                test: /\.html$/,
                exclude: /node_modules/,
                loader: 'file-loader?name=[name].[ext]'
            },
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                use: {
                    loader: 'elm-webpack-loader',
                    options: {
                        debug: true
                    }
                }
            }
        ]
    },

    mode: 'development',

    devServer: {
        inline: true,
        hot: true,
        stats: 'errors-only',
        historyApiFallback: true,
    }
};
